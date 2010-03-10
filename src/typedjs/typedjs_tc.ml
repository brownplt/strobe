open Prelude
open Typedjs_syntax
open Typedjs_types 
open Typedjs_pretty
open FormatExt
open Format

module JS = JavaScript_syntax (* needed for operators *)

let string_of_typ = pretty_string pretty_typ

let tc_const (const : Exprjs_syntax.const) = match const with
    Exprjs_syntax.CString _ -> typ_str
  | Exprjs_syntax.CRegexp _ -> typ_regexp
  | Exprjs_syntax.CNum _ -> typ_num
  | Exprjs_syntax.CInt _ -> typ_int
  | Exprjs_syntax.CBool _ -> typ_bool
  | Exprjs_syntax.CNull -> typ_bool
  | Exprjs_syntax.CUndefined -> typ_undef

let tc_arith (p : pos) (t1 : typ) (t2 : typ) (int_args : bool) 
    (num_result : bool) : typ =
  let result_typ = 
    if num_result = true then typ_num
    else if subtype t1 typ_int then t2
    else t1 in
    if (int_args && subtype t1 typ_int && subtype t2 typ_int) ||
      (not int_args && subtype t1 typ_num && subtype t2 typ_num) then
      result_typ
    else 
      raise
        (Typ_error 
           (p, match int_args with
                true -> 
                  sep [ text "operator expects arguments of type Int,";
                        text "but arguments have type"; p_typ t1; text "and";
                        p_typ t2 ] Format.str_formatter;
                  Format.flush_str_formatter ()
              | false -> 
                  sep [ text "operator expects Int or Double arguments,";
                        text "but arguments have type"; p_typ t1; text "and";
                        p_typ t2 ] Format.str_formatter;
                  Format.flush_str_formatter ()))

let tc_cmp (p : pos) (lhs : typ) (rhs : typ) : typ = 
  if (subtype lhs typ_str && subtype rhs typ_str) || 
    (subtype lhs typ_num && subtype rhs typ_num) then
      typ_bool
  else
    raise (Typ_error (p, "comparision type error"))

let rec tc_exp (env : Env.env) exp = match exp with
    EConst (_, c) -> tc_const c
  | EId (p, x) -> begin
      try 
        Env.lookup_id x env
      with Not_found -> raise (Typ_error (p, x ^ " is not defined"))
    end
  | ELet (_, x, e1, e2) -> tc_exp (Env.bind_id x (tc_exp env e1) env) e2
  | ESeq (_, e1, e2) -> begin match tc_exp env e1 with
        TBot -> (* e1 will not return; no need to typecheck e2 *)
          TBot
      | _ -> tc_exp env e2
    end
  | ERef (p, e) -> TRef (tc_exp env e)
  | EDeref (p, e) -> begin match tc_exp env e with
      | TRef t -> t
      | t -> failwith "TypedJS bug: deref failure"
    end 
  | ESetRef (p, e1, e2) -> begin match tc_exp env e1, tc_exp env e2 with
      | TRef s, t -> 
          if subtype s t && subtype t s then t
          else raise 
            (Typ_error 
               (p, sprintf "%s : left-hand side has type %s, but the \
                  right-hand side has type %s" (string_of_position p)
                  (string_of_typ s) (string_of_typ t)))
      | s, t -> failwith "TypedJS bug: erroneous ESetRef"
    end
  | ELabel (p, l, t, e) -> 
      let s = tc_exp (Env.bind_lbl l t env) e in
        if subtype s t then t
        else raise (Typ_error (p, "label type mismatch"))
  | EBreak (p, l, e) ->
      let s = 
        try Env.lookup_lbl l env
        with Not_found -> 
          raise (Typ_error (p, "label " ^ l ^ " is not defined"))
      and t = tc_exp env e in
        if subtype t s then TBot
        else raise
          (Typ_error 
             (p,
              match l with
                  "%return" -> sprintf 
                    "this expression has type %s, but the function\'s return \
                     type is %s" (string_of_typ t) (string_of_typ s)
                | _ -> (* This should not happen. Breaks to labels always have 
                          type typ_undef *)
                    sprintf "this expression has type %s, but the label %s has \
                          type %s" (string_of_typ t) l (string_of_typ s)))
  | ETryCatch (_, e1, x, e2) ->
      let t1 = tc_exp env e1
      and t2 = tc_exp (Env.bind_id x TTop env) e2 in
        typ_union t1 t2
  | ETryFinally (_, e1, e2) -> 
      let _ = tc_exp env e1 in
        tc_exp env e2
  | EThrow (_, e) -> 
      let _ = tc_exp env e in
        TBot
  | ETypecast (p, rt, e) -> Typedjs_lattice.static rt (tc_exp env e)
  | EArray (p, []) -> 
      raise (Typ_error (p, "an empty array literal requires a type annotation"))
  | EArray (_, e :: es) -> 
      let u = fold_left typ_union (tc_exp env e) (tc_exps env es) in
        TApp ("Array", [u])
  | EIf (p, e1, e2, e3) -> begin
      match tc_exp env e1, tc_exp env e2, tc_exp env e3 with
          TApp ("Boolean", []), t2, t3 -> typ_union t2 t3
        | t, _, _ -> raise (Typ_error (p, "expected a boolean"))
    end
  | EObject (p, fields) ->
      let tc_field (x, is_mutable, e) = 
        let t = tc_exp env e in
          (x, if is_mutable then TRef t else t) in
        typ_permute (TObject (map tc_field fields))
  | EBracket (p, obj, field) -> begin match tc_exp env obj, field with
        TObject fs, EConst (_, Exprjs_syntax.CString x) -> 
          (try
             snd2 (List.find (fun (x', _) -> x = x') fs)
           with Not_found ->
             raise (Typ_error (p, "the field " ^ x ^ " does not exist")))
      | TDom, _ -> TDom (* TODO: banned fields? *)
      | t, EConst (_, Exprjs_syntax.CString _) ->
          raise (Typ_error
                   (p, "expected an object, received " ^ string_of_typ t))
      | _ -> 
          raise (Typ_error (p, "field-lookup requires a string- literal"))
    end
  | EThis p -> begin
      try 
        Env.lookup_id "this" env
      with Not_found -> raise (Typ_error (p, "'this' used in non-func"))
    end
  | ENew (p, constr, args) -> begin match constr with
        ETypecast (_, _, EId (_, cid))
      | EId (_, cid) -> 
          (* treat it as a function application, but return the class
             type instead of the return type *)
          let class_typ = Env.lookup_class cid env in
          let arg_typs = tc_exps env args in
            tc_exp env (EApp (p, EId (p, cid), args));
            class_typ
      | _ -> raise (Typ_error (p, 
                               begin
                                 sep [ text "new must have ID as constructor";
                                       text " expression, but got";
                                       p_exp constr ] Format.str_formatter;
                                 Format.flush_str_formatter ()
                               end))

    end
  | EPrefixOp (p, op, e) -> begin match op, tc_exp env e with
        JS.PrefixLNot, TApp ("Boolean", []) -> typ_bool
      | JS.PrefixBNot, TApp ("Int", []) -> typ_int
      | JS.PrefixBNot, TApp ("Number", []) ->
          (** JavaScript converts floats to integers before doing a bitwise
              negation. E.g. [~(1.0)], [~1], and [-2] are equivalent. *)
          typ_int 
      | JS.PrefixPlus, TApp ("Int", []) -> typ_int
      | JS.PrefixPlus, TApp ("Number", []) -> typ_num 
      | JS.PrefixMinus, TApp ("Int", []) -> typ_int
      | JS.PrefixMinus, TApp ("Number", []) -> typ_num 
      | JS.PrefixMinus, t -> 
          raise 
            (Typ_error 
               (p, "- (minus) expects an Int/Double operand; received " ^
                  string_of_typ t))
      | JS.PrefixTypeof, _ -> typ_str
      | JS.PrefixVoid, _ -> typ_undef
          (* | PrefixDelete *)
    end
   | EInfixOp (p, op, e1, e2) -> 
       let t1 = tc_exp env e1
       and t2 = tc_exp env e2 in
       begin match op with
           JS.OpLT -> tc_cmp p t1 t2
         | JS.OpLEq -> tc_cmp p t1 t2
         | JS.OpGT -> tc_cmp p t1 t2
         | JS.OpGEq -> tc_cmp p t1 t2
         | JS.OpIn -> failwith "OpIn NYI"
         | JS.OpInstanceof -> failwith "instanceof NYI"
         | JS.OpEq -> typ_bool
         | JS.OpNEq -> typ_bool
         | JS.OpStrictEq -> typ_bool
         | JS.OpStrictNEq -> typ_bool
         | JS.OpLAnd -> typ_union t2 typ_bool
         | JS.OpLOr -> typ_union t1 t2
         | JS.OpMul -> tc_arith p t1 t2 false false
         | JS.OpDiv -> tc_arith p t1 t2 false true
         | JS.OpMod -> tc_arith p t1 t2 false true
         | JS.OpSub -> tc_arith p t1 t2 false false
         | JS.OpLShift -> tc_arith p t1 t2 true false
         | JS.OpSpRShift -> tc_arith p t1 t2 true false
         | JS.OpZfRShift -> tc_arith p t1 t2 true false
         | JS.OpBAnd -> tc_arith p t1 t2 true false
         | JS.OpBXor -> tc_arith p t1 t2 true false
         | JS.OpBOr -> tc_arith p t1 t2 true false
         | JS.OpAdd -> begin match subtype t1 typ_str, subtype t2 typ_str with
               true, _ -> typ_str
             | _, true -> typ_str
             | _ -> tc_arith p t1 t2 false false
           end
       end
  | EApp (p, f, args) -> begin match tc_exp env f with
        TArrow (_, expected_typs, result_typ) ->
          let arg_typs = tc_exps env args in
            if subtypes arg_typs expected_typs then result_typ 
            else if List.length args = List.length expected_typs then
              let typ_pairs = List.combine arg_typs expected_typs in
              let arg_ix = ref 1 in
              let find_typ_err (arg_typ, expected_typ) = 
                if subtype arg_typ expected_typ then (incr arg_ix; false)
                else true in
              let arg_typ, expected_typ = List.find find_typ_err typ_pairs in
                raise (Typ_error 
                         (p, sprintf "argument %d has type %s, but the \
                               function expects an argument of type %s" 
                            !arg_ix (string_of_typ arg_typ)
                               (string_of_typ expected_typ)))
            else raise (Typ_error 
                          (p, sprintf "arity-mismatch: the function expects %d \
                                arguments, but %d arguments given"
                             (List.length expected_typs) (List.length args)))
      | TDom -> 
          let arg_typs = tc_exps env args in
            if List.for_all (fun t -> subtype t TDom) arg_typs then
              TDom
            else
              raise (Typ_error (p, "DOM-function application"))
      | _ -> raise (Typ_error (p, "expected a function"))
    end
  | ERec (binds, body) -> 
      let f env (x, t, _) = Env.bind_id x t env in
      let env = fold_left f env binds in
      let tc_bind (x, t, e)=
        let s = tc_exp env e in
          if subtype s t then ()
          else (* this should not happen; rec-annotation is a copy of the
                  function's type annotation. *)
            failwith (sprintf "%s is declared to have type %s, but the bound \
                             expression has type %s" x (string_of_typ t)
                        (string_of_typ s)) in
        List.iter tc_bind binds;
        tc_exp env body
  | EFunc (p, args, fn_typ, body) -> begin match fn_typ with
        TArrow (_, arg_typs, result_typ) ->
          if List.length arg_typs = List.length args then ()
          else raise (Typ_error (p,
            "given " ^ (string_of_int (List.length args)) ^ " arg names but "
            ^ (string_of_int (List.length arg_typs)) ^ " arg types"));

          let bind_arg env x t = Env.bind_id x t env in
          let env = List.fold_left2 bind_arg env args arg_typs in
          let env = Env.clear_labels env in
          let body_typ = tc_exp env body in
            if subtype body_typ result_typ then fn_typ
            else raise (Typ_error
                          (p,
                           sprintf "function body has type %s, but the \
                             return type is %s" (string_of_typ body_typ)
                             (string_of_typ result_typ)))
      | _ -> raise (Typ_error (p, "invalid type annotation on a function"))
    end

and tc_exps env es = map (tc_exp env) es

let rec tc_def env def = match def with
    DEnd -> ()
  | DExp (e, d) -> 
      let _ = tc_exp env e in
        tc_def env d
  | DLet (p, x, e1, d2) -> tc_def (Env.bind_id x (tc_exp env e1) env) d2
  | DRec (binds, d) ->
      let f env (x, t, _) = Env.bind_id x t env in
      let env = fold_left f env binds in
      let tc_bind (x, t, e) =
        let s = tc_exp env e in
          if subtype s t then ()
          else (* this should not happen; rec-annotation is a copy of the
                  function's type annotation. *)
            failwith (sprintf "%s is declared to have type %s, but the bound \
                             expression has type %s" x (string_of_typ t)
                        (string_of_typ s)) in
        List.iter tc_bind binds;
        tc_def env d
  | DConstructor (cexp, d) -> let p = cexp.constr_pos in 
      begin match cexp.constr_typ with
          TArrow (_, arg_typs, result_typ) -> 
            if List.length arg_typs = List.length (cexp.constr_args) then ()
            else raise (
              Typ_error (p,
                         "given " ^ (string_of_int (List.length 
                                                      (cexp.constr_args))) 
                         ^ " arg names but "
                         ^ (string_of_int (List.length arg_typs)) 
                         ^ " arg types"));            
            let bind_arg env x t = Env.bind_id x t env in
            let env =List.fold_left2 bind_arg env (cexp.constr_args) arg_typs in
            let env = Env.clear_labels env in           
              begin match result_typ with
                  TObject (fields) -> 
                    List.iter (fun (n,t) -> 
                                 eprintf "init: %s\n" n) cexp.constr_inits;
                    (* first make sure all fields are initialized with
                       the right types in constr_inits. *)
                    let check_field (name, typ) = begin 
                      eprintf "checking field %s\n" name;
                      try 
                        let (_, e) = List.find (fun (n,_) -> n = name) 
                          cexp.constr_inits in 
                        let etype = tc_exp env e in
                          if subtype etype typ then () 
                          else raise (
                            Typ_error (
                              p,sprintf"for field %s, expected type %s, got %s" 
                                name (string_of_typ typ) (string_of_typ etype)))
                      with
                          Not_found -> raise (
                            Typ_error (p, "field " ^ name ^ 
                                         " not initialized in constructor"))
                    end in
                      List.iter check_field fields;
                      (* now check the body, with "this" bound to res_type *)
                      ignore (tc_exp (Env.bind_id "this" result_typ env) 
                        cexp.constr_exp);
                      (* create a new class, add fields as initial methods *)
                      (* also add the constr itself as a tarrow *)
                      let env = Env.new_class cexp.constr_name env in
                      let env = Env.bind_id cexp.constr_name 
                        cexp.constr_typ env in
                      let f (fname, ftype) envacc = Env.add_method
                        cexp.constr_name fname ftype envacc in
                      let env = fold_right f fields env in
                        tc_def env d
                | _ -> raise (Typ_error (
                                p, "constructor's ret type must be obj"))
              end
        | _ -> raise (Typ_error (p, "expected arrow type on constructor"))
      end
        
let typecheck = tc_def

(* 
type constr_exp = { 
  constr_pos : pos;
  constr_name : id;
  constr_typ : typ;
  constr_args : id list;
  constr_inits : (id * exp) list;
  constr_exp : exp;
  constr_prototype : exp
}

  | EFunc (p, args, fn_typ, body) -> begin match fn_typ with
        TArrow (_, arg_typs, result_typ) ->
          if List.length arg_typs = List.length args then ()
          else raise (Typ_error (p,
            "given " ^ (string_of_int (List.length args)) ^ " arg names but "
            ^ (string_of_int (List.length arg_typs)) ^ " arg types"));

          let bind_arg env x t = Env.bind_id x t env in
          let env = List.fold_left2 bind_arg env args arg_typs in
          let env = Env.clear_labels env in
          let body_typ = tc_exp env body in
            if subtype body_typ result_typ then fn_typ
            else raise (Typ_error
                          (p,
                           sprintf "function body has type %s, but the \
                             return type is %s" (string_of_typ body_typ)
                             (string_of_typ result_typ)))
      | _ -> raise (Typ_error (p, "invalid type annotation on a function"))
    end


*)

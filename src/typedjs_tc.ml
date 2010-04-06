open Prelude
open Typedjs_syntax
open Typedjs_env
open Typedjs_types 
open Format

module JS = JavaScript_syntax (* needed for operators *)

let error p s = raise (Typ_error (p, s))

let string_of_typ = FormatExt.to_string Typedjs_syntax.Pretty.p_typ

let tc_const (const : JavaScript_syntax.const) = match const with
    JavaScript_syntax.CString _ -> typ_str
  | JavaScript_syntax.CRegexp _ -> typ_regexp
  | JavaScript_syntax.CNum _ -> typ_num
  | JavaScript_syntax.CInt _ -> typ_int
  | JavaScript_syntax.CBool _ -> typ_bool
  | JavaScript_syntax.CNull -> typ_bool
  | JavaScript_syntax.CUndefined -> typ_undef

let tc_arith env (p : pos) (t1 : typ) (t2 : typ) (int_args : bool) 
    (num_result : bool) : typ =
  let subtype = Env.subtype env in
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
              | true -> 
                  fprintf str_formatter
                    "operator expects arguments of type Int, but \
                     arguments have type %a and %a" Pretty.pp_typ t1
                     Pretty.pp_typ t2; flush_str_formatter ()
              | false -> 
                  fprintf str_formatter
                    "operator expects arguments of type Number, but \
                     arguments have type %a and %a" 
                    Pretty.pp_typ t1 Pretty.pp_typ t2; flush_str_formatter ()))

let tc_cmp env (p : pos) (lhs : typ) (rhs : typ) : typ = 
  let subtype = Env.subtype env in
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
  | ERef (p, k, e) ->
      let t = tc_exp env e in
        begin match k with
          | SourceCell -> TSource t
          | SinkCell -> TSink t
          | RefCell -> TRef t
        end
  | EDeref (p, e) -> begin match tc_exp env e with
      | TRef t -> t
      | TSource t -> t
      | t -> raise (Typ_error (p, "cannot read an expression of type " ^
                                 (string_of_typ t)))
    end 
  | ESetRef (p, e1, e2) -> begin match tc_exp env e1, tc_exp env e2 with
      | TRef s, t
      | TSink s, t ->
          if Env.subtype env t s then 
            t
          else raise
            (Typ_error 
               (p, sprintf "left-hand side has type %s, but the \
                  right-hand side has type %s"
                  (string_of_typ s) (string_of_typ t)))
      | s, _ -> 
          raise (Typ_error 
                   (p, sprintf "cannot write to LHS (type %s)" 
                      (string_of_typ s)))
    end
  | ELabel (p, l, t, e) -> 
      let t = Env.check_typ p env t in
      let s = tc_exp (Env.bind_lbl l (Env.check_typ p env t) env) e in
        if Env.subtype env s t then t
        else raise (Typ_error (p, "label type mismatch"))
  | EBreak (p, l, e) ->
      let s = 
        try Env.lookup_lbl l env
        with Not_found -> 
          raise (Typ_error (p, "label " ^ l ^ " is not defined"))
      and t = tc_exp env e in
        if Env.subtype env t s then TBot
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
        Env.typ_union env t1 t2
  | ETryFinally (_, e1, e2) -> 
      let _ = tc_exp env e1 in
        tc_exp env e2
  | EThrow (_, e) -> 
      let _ = tc_exp env e in
        TBot
  | ETypecast (p, rt, e) -> 
      Env.static env rt (tc_exp env e)
  | EEmptyArray (p, elt_typ) -> 
      TConstr ("Array", [ Env.check_typ p env elt_typ ])
  | EArray (p, []) -> 
      raise (Typ_error (p, "an empty array literal requires a type annotation"))
  | EArray (p, e :: es) -> 
      let u = fold_left (Env.typ_union env) 
        (tc_exp env e) (tc_exps env es) in
        (* hack to make arrays not have undefined elements: *)
        Env.check_typ p env (TConstr ("Array", [u]))
  | EIf (p, e1, e2, e3) -> begin
      match tc_exp env e1, tc_exp env e2, tc_exp env e3 with
          TConstr ("Boolean", []), t2, t3 -> Env.typ_union env t2 t3
        | t, _, _ -> raise (Typ_error (p, "expected a boolean"))
    end
  | EObject (p, fields) ->
      Env.check_typ p env (TObject (map (second2 (tc_exp env)) fields))
  | EBracket (p, obj, field) -> begin match tc_exp env obj, field with
      | TObject fs, EConst (_, JavaScript_syntax.CString x) -> 
          (try
             snd2 (List.find (fun (x', _) -> x = x') fs)
           with Not_found ->
             raise (Typ_error (p, "the field " ^ x ^ " does not exist")))
      | TConstr ("Array", [tarr]), eidx ->
          let tidx = tc_exp env eidx in
            begin match tidx with
              | TConstr ("Int", []) -> tarr
              | _ -> error p 
                  ("array index requires Int, got " ^ string_of_typ tarr)
            end
      | TConstr (cname, []), EConst (_, JavaScript_syntax.CString fname) ->
          begin match Env.field_typ env cname fname with
            | Some t -> t
            | None -> error p ("object does not have a field " ^ fname)
          end
      | t, EConst (_, JavaScript_syntax.CString _) ->
          error p ("expected object, but got " ^ string_of_typ t)
      | _ -> 
          error p "field-lookup requires a string literal"
    end
  | EThis p -> begin
      try 
        Env.lookup_id "this" env
      with Not_found -> raise (Typ_error (p, "'this' used in non-func"))
    end
  | ENew (p, cid, args) ->
      (* treat it as a function application, but return the class
         type instead of the return type *)
      (if not (Env.is_class env cid) then error p ("undefined class: " ^ cid));
      let _ = tc_exp env (EApp (p, EId (p, cid), args)) in
        TConstr (cid, [])
  | EPrefixOp (p, op, e) -> begin match op, tc_exp env e with
      | JS.PrefixLNot, TConstr ("Boolean", []) -> typ_bool
      | JS.PrefixLNot, t -> 
          raise (Typ_error (p, "! (logical not) expects a number; received " ^
                              (string_of_typ t)))
      | JS.PrefixBNot, TConstr ("Int", []) -> typ_int
      | JS.PrefixBNot, TConstr ("Number", []) ->
          (** JavaScript converts floats to integers before doing a bitwise
              negation. E.g. [~(1.0)], [~1], and [-2] are equivalent. *)
          typ_int 
      | JS.PrefixBNot, t ->
          raise (Typ_error (p, "~ (bitwise not) expects a number; received " ^
                              (string_of_typ t)))
      | JS.PrefixPlus, TConstr ("Int", []) -> typ_int
      | JS.PrefixPlus, TConstr ("Number", []) -> typ_num 
      | JS.PrefixPlus, t ->
          raise (Typ_error (p, "+ (plus) expects a number; received " ^
                              (string_of_typ t)))
      | JS.PrefixMinus, TConstr ("Int", []) -> typ_int
      | JS.PrefixMinus, TConstr ("Number", []) -> typ_num 
      | JS.PrefixMinus, t -> 
          raise 
            (Typ_error 
               (p, "- (minus) expects an Int/Double operand; received " ^
                  string_of_typ t))
      | JS.PrefixTypeof, _ -> typ_str
      | JS.PrefixVoid, _ -> typ_undef
      | JS.PrefixDelete, _ -> failwith "PrefixDelete NYI"
    end
   | EInfixOp (p, op, e1, e2) -> 
       let t1 = tc_exp env e1
       and t2 = tc_exp env e2 in
       begin match op with
           JS.OpLT -> tc_cmp env p t1 t2
         | JS.OpLEq -> tc_cmp env p t1 t2
         | JS.OpGT -> tc_cmp env p t1 t2
         | JS.OpGEq -> tc_cmp env p t1 t2
         | JS.OpIn -> failwith "OpIn NYI"
         | JS.OpInstanceof -> failwith "instanceof NYI"
         | JS.OpEq -> typ_bool
         | JS.OpNEq -> typ_bool
         | JS.OpStrictEq -> typ_bool
         | JS.OpStrictNEq -> typ_bool
         | JS.OpLAnd -> Env.typ_union env t2 typ_bool
         | JS.OpLOr -> Env.typ_union env t1 t2
         | JS.OpMul -> tc_arith env p t1 t2 false false
         | JS.OpDiv -> tc_arith env p t1 t2 false true
         | JS.OpMod -> tc_arith env p t1 t2 false true
         | JS.OpSub -> tc_arith env p t1 t2 false false
         | JS.OpLShift -> tc_arith env p t1 t2 true false
         | JS.OpSpRShift -> tc_arith env p t1 t2 true false
         | JS.OpZfRShift -> tc_arith env p t1 t2 true false
         | JS.OpBAnd -> tc_arith env p t1 t2 true false
         | JS.OpBXor -> tc_arith env p t1 t2 true false
         | JS.OpBOr -> tc_arith env p t1 t2 true false
         | JS.OpAdd -> begin match Env.subtype env t1 typ_str,
             Env.subtype env t2 typ_str with
                 true, _ -> typ_str
               | _, true -> typ_str
               | _ -> tc_arith env p t1 t2 false false
           end
       end
  | EApp (p, f, args) -> begin match tc_exp env f with
        TArrow (_, expected_typs, result_typ) ->
          let arg_typs = tc_exps env args in
            if Env.subtypes env arg_typs expected_typs 
            then result_typ 
            else if List.length args = List.length expected_typs then
              let typ_pairs = List.combine arg_typs expected_typs in
              let arg_ix = ref 1 in
              let find_typ_err (arg_typ, expected_typ) = 
                if Env.subtype env arg_typ expected_typ 
                then (incr arg_ix; false)
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
      | t -> 
          raise 
            (Typ_error 
               (p, "expected a function, but expression has type " ^
                  (string_of_typ t)))
    end
  | ERec (binds, body) -> 
      let f env (x, t, e) =
        Env.bind_id x (Env.check_typ (Exp.pos e) env t) env in
      let env = fold_left f env binds in
      let tc_bind (x, t, e)=
        let s = tc_exp env e in
          if Env.subtype env s t then ()
          else (* this should not happen; rec-annotation is a copy of the
                  function's type annotation. *)
            failwith (sprintf "%s is declared to have type %s, but the bound \
                             expression has type %s" x (string_of_typ t)
                        (string_of_typ s)) in
        List.iter tc_bind binds;
        tc_exp env body
  | EFunc (p, args, fn_typ, body) -> begin match Env.check_typ p env fn_typ with
        TArrow (_, arg_typs, result_typ) ->
          if List.length arg_typs = List.length args then ()
          else raise (Typ_error (p,
            "given " ^ (string_of_int (List.length args)) ^ " arg names but "
            ^ (string_of_int (List.length arg_typs)) ^ " arg types"));

          let bind_arg env x t = Env.bind_id x t env in
          let env = List.fold_left2 bind_arg env args arg_typs in
          let env = Env.clear_labels env in
          let body_typ = tc_exp env body in
            if Env.subtype env body_typ result_typ then fn_typ
            else raise 
              (Typ_error
                 (p,
                  sprintf "function body has type\n%s\n, but the \
                             return type is\n%s" (string_of_typ body_typ)
                    (string_of_typ result_typ)))
      | _ -> raise (Typ_error (p, "invalid type annotation on a function"))
    end
  | ESubsumption (p, t, e) ->
      let s = tc_exp env e in
      let t = Env.check_typ p env t in
        if Env.subtype env s t then
          t
        else 
          raise (Typ_error (p, "subsumption error"))
  | EDowncast (p, t, e) ->  ignore (tc_exp env e); Env.check_typ p env t
        

and tc_exps env es = map (tc_exp env) es

let rec tc_def env def = match def with
    DEnd -> ()
  | DExp (e, d) -> 
      let _ = tc_exp env e in
        tc_def env d
  | DLet (p, x, e1, d2) -> tc_def (Env.bind_id x (tc_exp env e1) env) d2
  | DRec (binds, d) ->
      let f env (x, t, e) = 
        Env.bind_id x (Env.check_typ (Exp.pos e) env t) env in
      let env = fold_left f env binds in
      let tc_bind (x, t, e) =
        let s = tc_exp env e in
          if Env.subtype env s t then ()
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
                  TObject fields -> 
                    (* first make sure all fields are initialized with
                       the right types in constr_inits. *)
                    let check_field (name, typ) = match typ with
                      | TRef t -> begin try 
                          let (_, e) = List.find (fun (n,_) -> n = name) 
                            cexp.constr_inits in 
                          let etype = tc_exp env e in
                            if Env.subtype env etype t then () 
                            else 
                              raise 
                                (Typ_error (
                                   p, sprintf "for field %s, expected type %s, \
                                               got %s" name (string_of_typ t)
                                     (string_of_typ etype)))
                        with
                            Not_found -> raise (
                              Typ_error (p, "field " ^ name ^ 
                                           " not initialized in constructor"))
                        end
                      | _ ->
                          raise (Typ_error (p, "expected an arrow type for \
                            the constructor")) in
                      List.iter check_field fields;
                      (* now check the body, with "this" bound to res_type *)
                      ignore (tc_exp (Env.bind_id "this" result_typ env) 
                        cexp.constr_exp);
                      (* create a new class, add fields as initial methods *)
                      (* also add the constr itself as a tarrow *)
                      let env = Env.new_root_class env cexp.constr_name in
                      let env = Env.bind_id cexp.constr_name 
                        (Env.check_typ p env cexp.constr_typ) env in
                      let f (fname, ftype) envacc = Env.add_method
                        cexp.constr_name fname ftype envacc in
                      let env = fold_right f fields env in
                        tc_def env d
                | _ -> raise (Typ_error (
                                p, "constructor's ret type must be obj"))
              end
        | _ -> raise (Typ_error (p, "expected arrow type on constructor"))
      end
  | DExternalMethod (p, cname, mid, me, d) -> 
      try
        let expenv = Env.bind_id "this" (TConstr (cname, [])) env in
        let env' = Env.add_method cname mid (TRef (tc_exp expenv me)) env in
          tc_def env' d
      with Not_found -> raise (
        Typ_error (p, "class " ^ cname ^ " doesnt exist"))
        
let typecheck = tc_def



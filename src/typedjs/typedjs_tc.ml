open Prelude
open Typedjs_syntax
open Typedjs_types 
open Typedjs_pretty
open Typedjs_stxutil

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
                  sprintf "operator expects arguments of type Int, but \
                    arguments have type %s and %s" (string_of_typ t1) 
                    (string_of_typ t2)
              | false -> 
                  sprintf "operator expects Int or Double arguments, but \
                    arguments have type %s and %s" (string_of_typ t1)
                    (string_of_typ t2)))

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
  | ELet (_, x, e1, e2) ->
      let t = tc_exp env e1
      and x_available = not (IdSet.mem x (local_av_exp e2)) in
      let env = Env.bind_id x t env in
      let env = Env.remove_assigned_ids (av_exp e1) env in
      let env = if x_available then Env.new_assignable_id x env else env in
        tc_exp env e2
  | ESeq (_, e1, e2) -> begin match tc_exp env e1 with
        TBot -> (* e1 will not return; no need to typecheck e2 *)
          TBot
      | _ -> tc_exp env e2
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
(*  | ETypecast (p, rt, e) ->
      static rt (tc_exp env e) *)
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
(*  | EThis of 'a *)
(*| ENew of 'a * 'a exp * 'a exp list *)
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
      let bind_avs = map (fun (_, _, e) -> av_exp e) binds in
      let body_env = Env.remove_assigned_ids (IdSetExt.unions bind_avs) env in
      let rec mk_bind_envs (bind_avs : IdSet.t list) (prev_av : IdSet.t) 
          : Env.env list = match bind_avs with
          [] -> []
        | bind_av :: next_avs -> 
            let bind_env = 
              Env.remove_assigned_ids (IdSetExt.unions (prev_av :: next_avs))
                env in
            let prev_av' = IdSet.union bind_av prev_av in
              bind_env :: (mk_bind_envs next_avs prev_av') in
      let bind_envs = mk_bind_envs bind_avs IdSet.empty in
      let tc_bind (x, t, e) bind_env =
        let s = tc_exp bind_env e in
          if subtype s t then ()
          else (* this should not happen; rec-annotation is a copy of the
                  function's type annotation. *)
            failwith (sprintf "%s is declared to have type %s, but the bound \
                             expression has type %s" x (string_of_typ t)
                        (string_of_typ s)) in
        List.iter2 tc_bind binds bind_envs;
        tc_exp body_env body
  | EFunc (p, args, fn_typ, body) -> begin match fn_typ with
        TArrow (_, arg_typs, result_typ) ->
          if List.length arg_typs = List.length args then ()
          else raise (Typ_error (p, "not all arguments have types"));
          let bind_arg env (x, t) = 
            (* all arguments are available for dataflow in the body *)
            Env.new_assignable_id x (Env.bind_id x t env) in
          let env' = fold_left bind_arg env (List.combine args arg_typs) in
          let body' = (* annotate env' *) body in
            (* The env. in which we type the annotated body specifies the
               env. in which we do dataflow for nested functions. Remove the
               locally assigned identifiers from this env. (accounts for
               assigning to arguments.) *)
          let env' = Env.remove_assigned_ids (local_av_exp body') env' in
          let env' = Env.clear_labels env' in
          let body_typ = tc_exp env' body' in
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
  | DLet (p, x, e1, d2) ->
      let t = tc_exp env e1
      and x_available = not (IdSet.mem x (local_av_def d2)) in
      let env = Env.bind_id x t env in
      let env = Env.remove_assigned_ids (av_exp e1) env in
      let env = if x_available then Env.new_assignable_id x env else env in
        tc_def env d2
  | DRec (binds, d) ->
      let f env (x, t, _) = Env.bind_id x t env in
      let env = fold_left f env binds in
      let bind_avs = map (fun (_, _, e) -> av_exp e) binds in
      let body_env = Env.remove_assigned_ids (IdSetExt.unions bind_avs) env in
      let rec mk_bind_envs (bind_avs : IdSet.t list) (prev_av : IdSet.t) 
          : Env.env list = match bind_avs with
          [] -> []
        | bind_av :: next_avs -> 
            let bind_env = 
              Env.remove_assigned_ids (IdSetExt.unions (prev_av :: next_avs))
                env in
            let prev_av' = IdSet.union bind_av prev_av in
              bind_env :: (mk_bind_envs next_avs prev_av') in
      let bind_envs = mk_bind_envs bind_avs IdSet.empty in
      let tc_bind (x, t, e) bind_env =
        let s = tc_exp bind_env e in
          if subtype s t then ()
          else (* this should not happen; rec-annotation is a copy of the
                  function's type annotation. *)
            failwith (sprintf "%s is declared to have type %s, but the bound \
                             expression has type %s" x (string_of_typ t)
                        (string_of_typ s)) in
        List.iter2 tc_bind binds bind_envs;
        tc_def body_env d

let typecheck = tc_def

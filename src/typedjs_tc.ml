open Prelude
open Typedjs_syntax
open Typedjs_env
open Format
open Typedjs_dyn
open Typedjs_tc_util

let contracts : (int * typ) IntMap.t ref = ref IntMap.empty

let error_on_unreachable = ref true

let disable_unreachable_check () =
  error_on_unreachable := false

let rec skip n l = if n == 0 then l else (skip (n-1) (List.tl l))
let rec fill n a l = if n <= 0 then l else fill (n-1) a (List.append l [a])

let error p s = raise (Typ_error (p, s))

let string_of_typ = FormatExt.to_string Typedjs_syntax.Pretty.p_typ

let un_null t = match t with
  | TUnion (TPrim (Undef), t') -> t'
  | TUnion (t', TPrim (Undef)) -> t'
  | TUnion (TPrim (Null), t') -> t'
  | TUnion (t', TPrim (Null)) -> t'
  | _ -> t

let un_ref t = match t with
  | TRef s -> s
  | TSource s -> s
  | TSink s -> s
  | _ -> failwith ("un_ref got " ^ string_of_typ t)
 
let rec tc_exp (env : Env.env) exp = match exp with
  | EConst (_, c) -> tc_const c
  | EBot _ -> TBot
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
      | TField -> TField
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
      let t = tc_exp env e in
        Env.static env rt t
  | EEmptyArray (p, elt_typ) -> 
      error p "Don't know what to do with arrays yet"
  | EArray (p, []) -> 
      raise (Typ_error (p, "an empty array literal requires a type annotation"))
  | EArray (p, e :: es) -> 
      error p "Don't know what to do with arrays yet"
  | EIf (p, e1, e2, e3) ->
      let c = tc_exp env e1 in
        if Env.subtype env c typ_bool then
          Env.typ_union env (tc_exp env e2) (tc_exp env e3)
        else
          error p ("expected condition to have type Bool, but got a " ^ 
                    (string_of_typ c))
  | EObject (p, fields) ->
      Env.check_typ p env (TObject (map (second2 (tc_exp env)) fields))
  | EBracket (p, obj, field) -> begin match un_null (tc_exp env obj), field with
      | TObject fs, EConst (_, JavaScript_syntax.CString x) -> 
          (try
             snd2 (List.find (fun (x', _) -> x = x') fs)
           with Not_found ->
             raise (Typ_error (p, "the field " ^ x ^ " does not exist")))
      | TField, field -> begin match tc_exp env field with
          | TField -> TField
          | _ -> error p "expected a TField index"
        end
      | t, EConst (_, JavaScript_syntax.CString _) ->
          error p ("expected object, but got " ^ string_of_typ t)
      | _ -> 
          error p "field-lookup requires a string literal"
    end
  | EUpdate (p, _, _, _) -> error p ("Haven't implemented update yet")
  | ENew (p, cid, args) -> error p "New doesn't work yet (constrs)"
  | EPrefixOp (p, op, e) -> tc_exp env (EApp (p, EId (p, op), [e]))
  | EInfixOp (p, "+", e1, e2) -> 
      let t1 = tc_exp env e1 in
      let t2 = tc_exp env e2 in
        if (t1 = (TPrim Str) || t2 = (TPrim Str)) then
          TPrim Str 
        else 
          tc_exp env (EApp (p, EId (p, "+"), [e1; e2]))
  | EInfixOp (p, op, e1, e2) -> tc_exp env (EApp (p, EId (p, op), [e1; e2]))
  | EApp (p, f, args) -> begin match un_null (tc_exp env f) with
      | TForall (x, _, TArrow (expected_typ, result_typ)) ->
          let subst = unify_typ (TArrow (expected_typ, result_typ))
            (TArrow (map (tc_exp env) args, result_typ)) in
            begin try
              let u = IdMap.find x subst in (* TODO: needless recomputation *)
                tc_exp env (EApp (p, ETypApp (p, f, u), args))
            with Not_found ->
              let t = TArrow (expected_typ, result_typ) in
              error p (sprintf "could not determine \'%s in the function type \
                                %s" x (string_of_typ t))
            end
      | TArrow (expected_typs, result_typ) ->
          let arg_typs' = map (tc_exp_ret env) args in
          let arg_typs = 
            fill (List.length expected_typs - List.length args) 
              (TPrim Undef) arg_typs' in
            if Env.subtypes env arg_typs expected_typs 
            then result_typ 
            else if List.length args = List.length expected_typs  
            then
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
  | EFunc (p, args, fn_typ, body) -> 
      let expected_typ = Env.check_typ p env fn_typ in
      begin match Env.bind_typ env expected_typ with
          (env, TArrow (arg_typs, result_typ)) ->
            if not (List.length arg_typs = List.length args) then
              error p 
                (sprintf "given %d argument names, but %d argument types"
                   (List.length args) (List.length arg_typs));
            let bind_arg env x t = Env.bind_id x t env in
            let env = List.fold_left2 bind_arg env args arg_typs in
            let env = Env.clear_labels env in
            let body_typ = tc_exp env body in
              if Env.subtype env body_typ result_typ then 
                expected_typ
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
  | EAssertTyp (p, raw_t, e) ->
      let s = tc_exp env e in
      let t = Env.check_typ p env raw_t in
        if Env.subtype env s t then
          s (* we do not subsume *)
        else
          error p 
            (sprintf "expression has type %s, which is incompatible with the \
                      annotation" (string_of_typ s))
  | EDowncast (p, t, e) -> 
      let t = Env.check_typ p env t in
      let (p1, p2) = Exp.pos e in 
        contracts := IntMap.add p1.Lexing.pos_cnum (p2.Lexing.pos_cnum, t)
          !contracts;
        ignore (tc_exp env e);
        t
  | ETypAbs (p, x, t, e) ->
      let t = Env.check_typ p env t in
      let env = Env.bind_typ_id x t env in
      TForall (x, t, tc_exp env e)
  | ETypApp (p, e, u) ->
      let u = Env.check_typ p env u in
        begin match tc_exp env e with
          | TForall (x, s, t) ->
              if Env.subtype env u s then
                typ_subst x u t
              else 
                error p (sprintf "expected an argument of type %s, got %s"
                           (string_of_typ s) (string_of_typ u))
          | t ->
              error p (sprintf "expected a quantified type (got %s)"
                         (string_of_typ t))
        end
  | EForInIdx _ -> TField
  | ECheat (p, t, _) -> t

and tc_exps env es = map (tc_exp env) es

(* type-check [e] and ensure that the resulting type is not [TBot]. If 
   [e : TBot], then [e] provably does not return. There are two possibilities:
   
   1. [e] is a control operator. If so, call [tc_exp'] to avoid this check.
   2. [e] is unreachable code. This is not a type-error, but probably 
      unintended.
*)
   
and tc_exp_ret env e = 
  let t = tc_exp env e in
    if !error_on_unreachable && t = TBot then
      error (Exp.pos e) "unreachable code"
    else 
      t

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
          TArrow (arg_typs, result_typ) -> 
            if List.length arg_typs = List.length (cexp.constr_args) then ()
            else raise (
              Typ_error (p,
                         "given " ^ (string_of_int (List.length 
                                                      (cexp.constr_args))) 
                         ^ " arg names but "
                         ^ (string_of_int (List.length arg_typs)) 
                         ^ " arg types"));            
            let bind_arg env x t = Env.bind_id x t env in
            let env = List.fold_left2 bind_arg env (cexp.constr_args) arg_typs
            in
            let env = Env.clear_labels env in           
              begin match result_typ with
                  TObject fields -> 
                    (* first update the env to have the class
                       available inside the constructor body *)
                    (* create a new class, add fields as initial methods *)
                    (* also add the constr itself as a tarrow *)
                    let env = Env.new_root_class env cexp.constr_name in
                    let env = Env.bind_id cexp.constr_name 
                      (TRef (Env.check_typ p env cexp.constr_typ)) env in
                    let f (fname, ftype) envacc = Env.add_method
                      cexp.constr_name fname ftype envacc in
                    let env = fold_right f fields env in
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
                      tc_def env d
                | _ -> raise (Typ_error (
                                p, "constructor's ret type must be obj"))
              end
        | _ -> raise (Typ_error (p, "expected arrow type on constructor"))
      end
  | DExternalMethod (p, cname, mid, me, d) -> 
       error p "Can't do external methods yet (constrs"

let typecheck = tc_def



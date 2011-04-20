open Prelude
open Typedjs_syntax
open Typedjs_env
open Format
open Typedjs_dyn
open Typedjs_tc_util

let consumed_owned_vars  = ref IdSet.empty

let contracts : (int * typ) IntMap.t ref = ref IntMap.empty

let array_idx_pat = 
  Sb_strPat.parse Lexing.dummy_pos
    "(([0-9])*|(\"+Infinity\"|(\"-Infinity\"|\"NaN\")))"


let mk_array_typ p env elt_typ =
  TRef (TObject
	  ([(array_idx_pat, PMaybe elt_typ);
	    (Sb_strPat.singleton "length", PPresent (TPrim Int))],
	   (TId "Array_proto")))
	  
let error_on_unreachable = ref true

let disable_unreachable_check () =
  error_on_unreachable := false

let is_flows_enabled = ref true

let disable_flows () = is_flows_enabled := false

let rec skip n l = if n == 0 then l else (skip (n-1) (List.tl l))
let rec fill n a l = if n <= 0 then l else fill (n-1) a (List.append l [a])

let error p s = raise (Typ_error (p, s))

let string_of_typ = FormatExt.to_string Typedjs_syntax.Pretty.p_typ
let string_of_exp = FormatExt.to_string Typedjs_syntax.Pretty.p_exp

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
 
let rec tc_exp (env : Env.env) (exp : exp) : typ = match exp with
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
  | ERef (p1, RefCell, EEmptyArray (p2, elt_typ)) -> 
      mk_array_typ p2 env elt_typ
  | ERef (p1, RefCell, EArray (p2, [])) -> 
    raise (Typ_error (p2, "an empty array literal requires a type annotation"))
  | ERef (p1, RefCell, EArray (p2, es)) ->
      begin match map (tc_exp env) es with
        | t1::ts ->
            let tarr = List.fold_right (Env.typ_union env) ts t1 in
              mk_array_typ p2 env tarr
        | [] -> failwith "desugar bug: unexpected empty array"
      end
  | EEmptyArray (p, _)
  | EArray (p, _) -> failwith (sprintf "desugar bug: array outside a ref at %s" 
				 (string_of_position p))
  | ERef (p, k, e) ->
      let t = tc_exp env e in
        begin match k with
          | SourceCell -> TSource t
          | SinkCell -> TSink t
          | RefCell -> TRef t
        end
  | EDeref (p, e) -> begin match simpl_typ env (tc_exp env e) with
      | TRef t -> t
      | TSource t -> t
      | TField -> TField
      | t -> raise (Typ_error (p, "cannot read an expression of type " ^
                                 (string_of_typ t)))
    end 
  | ESetRef (p, e1, e2) -> begin match simpl_typ env (tc_exp env e1), tc_exp env e2 with
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
      let s = tc_exp (Env.bind_lbl l t env) e in
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
  | EIf (p, e1, e2, e3) ->
      let c = tc_exp env e1 in
        if Env.subtype env c typ_bool then
          Env.typ_union env (tc_exp env e2) (tc_exp env e3)
        else
          error p ("expected condition to have type Bool, but got a " ^ 
                    (string_of_typ c))
  | EObject (p, fields) ->
      let mk_field (name, exp) = 
	(Sb_strPat.singleton name, PPresent (tc_exp env exp)) in
      (* TODO: everything else hsould be absent *)
      TObject (map mk_field fields, TId "Object")
  | EBracket (p, obj, field) -> 
    begin match simpl_typ env (un_null (tc_exp env obj)), 
      simpl_typ env (tc_exp env field) with
      | ((TObject (fs, proto)) as tobj), TRegex idx_pat -> 
          fields p env tobj idx_pat
      | TObject _, typ -> 
          error p (sprintf "Got %s rather than a regex in lookup"
                     (string_of_typ typ))
      | TField, TField -> TField
      | TField, _ -> error p "expected a TField index"
      | obj, field -> 
          error p (sprintf "Got %s[%s] in object lookup."
                     (string_of_typ obj) (string_of_typ field))
    end
  | EUpdate (p, obj, field, value) -> begin
      match simpl_typ env (tc_exp env obj), 
        simpl_typ env (tc_exp env field), tc_exp env value with
          | (TObject (fs, proto) as tobj), (TRegex idx_pat as tfld), typ ->
              let okfield (fld_pat, prop) = 
                if Sb_strPat.is_overlapped fld_pat idx_pat
                then match prop with
                  | PPresent s
                  | PMaybe s -> if Env.subtype env typ s then true
                    else error p (sprintf "%s not subtype of %s in %s[%s = %s]"
                                    (string_of_typ typ) (string_of_typ s) 
                                    (string_of_typ tobj) (string_of_typ tfld)
                                    (string_of_typ typ))
                  | PErr -> error p "Assigning to error field"
                  | PAbsent -> error p (sprintf "Assigning to absent field")
                else true in
                if List.for_all okfield fs then tobj
                else error p "Shouldn't happen --- unknown error in update"
          | obj, fld, typ ->
              error p (sprintf "Bad update: %s[%s = %s]"
                         (string_of_typ obj) (string_of_typ fld) 
                         (string_of_typ typ))
    end
  | ENew (p, cid, args) -> error p "New doesn't work yet (constrs)"
  | EPrefixOp (p, op, e) -> tc_exp env (EApp (p, EId (p, op), [e]))
  | EInfixOp (p, "+", e1, e2) -> 
    begin match (tc_exp env e1, tc_exp env e2) with
      | TRegex _, _
      | _, TRegex _ -> 
	TRegex Sb_strPat.all
      | t1, t2 -> 
	tc_exp env (EApp (p, EId (p, "+"), [ ECheat (p, t1, e1); 
					     ECheat (p, t2, e2) ]))
    end
  | EInfixOp (p, op, e1, e2) -> tc_exp env (EApp (p, EId (p, op), [e1; e2]))
  | EApp (p, f, args) -> 
      let rec check_app tfun =
        begin match tfun with 
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
                         (p, sprintf "argument %d: expected %s, but received %s"
                            !arg_ix (string_of_typ expected_typ)
                                    (string_of_typ arg_typ)))
                 else raise (Typ_error 
                               (p, sprintf "arity-mismatch: the function expects %d \
                                arguments, but %d arguments given"
                                  (List.length expected_typs) (List.length
                                  args)))
           | TIntersect (t1, t2) -> 
	     begin 
	       try 
		 let r1 = check_app t1 in 
		 begin 
		   try
                     let r2 = check_app t2 in
                     Env.typ_intersect env r1 r2
                   with Typ_error _ -> r1 
		 end
               with Typ_error _ -> check_app t2 
	     end
           | TForall (x, max_typ, (TArrow (expected_typ, result_typ) as ft)) ->
             let tfun' = TArrow (map (tc_exp env) args, result_typ) in
             let subst = unify_typ env ft tfun' in
             begin match subst with 
               | None -> error p (sprintf "unification failure\n: %s\n~ %s\n%s"
				    (string_of_typ ft)
				    (string_of_typ tfun')
				    (string_of_exp exp))
               | Some subst -> 
                 begin try
                         let u = IdMap.find x subst in
                         if Env.subtype env u max_typ then
                           check_app (typ_subst x u ft)
                         else
                           error p (sprintf "Unified, but to %s, which is not \
                                             a subtype of %s; type is %s\n%s"
				      (string_of_typ u)
				      (string_of_typ max_typ)
				      (string_of_typ tfun)
				      (string_of_exp exp))
                   with Not_found ->
                     error p (sprintf "could not determine \'%s in %s" 
                                x (string_of_typ ft))
                 end
             end
           | _ -> failwith ("Expected an arrow or intersection in EApp, got: " ^
                               (string_of_typ tfun))
        end in (match (un_null (tc_exp env f)) with
                  | t -> check_app t)
  | ERec (binds, body) -> 
      let f env (x, t, e) =
        Env.bind_id x t env in
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
        tc_exp env body
  | EFunc (p, args, func_info, body) -> 
    begin
      let misowned_vars = IdSet.inter !consumed_owned_vars 
        func_info.func_owned in
      if (not (IdSet.is_empty misowned_vars)) then
        raise (Typ_error (p, sprintf "identifier %s is already owned"
          (IdSet.choose misowned_vars)))
      else
        consumed_owned_vars := IdSet.union !consumed_owned_vars
          func_info.func_owned;
    end;
      let expected_typ = func_info.func_typ in
      begin match Env.bind_typ env expected_typ with
        | (env, TArrow (arg_typs, result_typ)) ->
            if not (List.length arg_typs = List.length args) then
              error p 
                (sprintf "given %d argument names, but %d argument types"
                   (List.length args) (List.length arg_typs));
            let bind_arg env x t = Env.bind_id x t env in
            let env = List.fold_left2 bind_arg env args arg_typs in
            let env = Env.clear_labels env in
            let body = 
              if !is_flows_enabled then
                Sb_semicfa.semicfa (func_info.func_owned) env body 
              else 
                body in
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
        if Env.subtype env s t then
          t
        else 
          raise (Typ_error (p, "subsumption error"))
  | EAssertTyp (p, t, e) ->
      let s = tc_exp env e in
        if Env.subtype env s t then
          s (* we do not subsume *)
        else
          error p 
            (sprintf "expression has type %s, which is incompatible with the \
                      annotation" (string_of_typ s))
  | EDowncast (p, t, e) -> 
      let (p1, p2) = Exp.pos e in 
        contracts := IntMap.add p1.Lexing.pos_cnum (p2.Lexing.pos_cnum, t)
          !contracts;
        ignore (tc_exp env e);
        t
  | ETypAbs (p, x, t, e) ->
      let env = Env.bind_typ_id x t env in
      TForall (x, t, tc_exp env e)
  | ETypApp (p, e, u) ->
    begin match tc_exp env e with
      | TForall (x, s, t) ->
        if Env.subtype env u s then
          typ_subst x u t
        else 
          error p (sprintf "expected an argument of type %s, got %s"
                     (string_of_typ s) (string_of_typ u))
      | t ->
        error p (sprintf "expected forall-type in type application, got:\
                                \n%s\nargument has type:\n%s"
		   (string_of_typ t) (string_of_typ u))
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

let typecheck env exp =
  let _ = tc_exp env exp in
  ()



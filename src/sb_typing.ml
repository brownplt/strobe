open Prelude
open Typedjs_syntax
open Typedjs_env
open Format
open Typedjs_dyn
open Typedjs_tc_util

let consumed_owned_vars  = ref IdSet.empty

let contracts : (int * typ) IntMap.t ref = ref IntMap.empty

let array_idx_pat = 
  P.parse Lexing.dummy_pos
    "(([0-9])*|(\"+Infinity\"|(\"-Infinity\"|\"NaN\")))"


let mk_array_typ p env elt_typ =
  TApp (TId "Array", [elt_typ])
	  
let error_on_unreachable = ref true

let disable_unreachable_check () =
  error_on_unreachable := false

let is_flows_enabled = ref true

let disable_flows () = is_flows_enabled := false

let rec skip n l = if n == 0 then l else (skip (n-1) (List.tl l))
let rec fill n a l = if n <= 0 then l else fill (n-1) a (List.append l [a])

let error p s = raise (Typ_error (p, s))

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

let check_kind p env typ : typ =
  match kind_check env typ with
    | KStar -> typ
    | k ->
      raise 
	      (Typ_error 
	         (p, sprintf "type term has kind %s; expected *" (string_of_kind k)))

let expose_simpl_typ env typ = expose env (simpl_typ env typ)
  
let rec tc_exp (env : env) (exp : exp) : typ = match exp with
  (* TODO: Pure if-splitting rule; make more practical by integrating with
      flow typing. *)
  | EIf (p, EInfixOp (_, "hasfield",  EDeref (_, EId (_, obj)), (EId (_, fld))),
	       true_part, false_part) ->
    begin match expose_simpl_typ env (lookup_id obj env), lookup_id fld env with
      | TRef (TObject ot), TRegex pat -> 
	      let subtract (p, t) =
	        if P.is_overlapped p pat then (P.subtract p pat, t) (* perf *)
	        else (p, t) in
	      let false_typ = tc_exp env false_part in
	      let true_typ =
	        let fld_typ = simpl_lookup (tid_env env) (TObject ot) pat in
	        let env = bind_typ_id "alpha" (TRegex pat) env in
	        let env = bind_id fld (TId "alpha") env in
	        let env = bind_id obj 
	          (TObject (mk_obj_typ ((P.var "alpha", PPresent fld_typ) ::
				                             map subtract (fields ot)))) env in
	        tc_exp env true_part in
	      typ_union env true_typ false_typ
      | s, t ->
	      raise (Typ_error (p, "expected object and string types, got " ^
	        string_of_typ s ^ " and " ^ string_of_typ t))
    end
  | EConst (_, c) -> tc_const c
  | EBot _ -> TBot
  | EId (p, x) -> begin
    try 
      lookup_id x env
    with Not_found -> raise (Typ_error (p, x ^ " is not defined")) (* severe *)
  end
  | ELet (_, x, e1, e2) -> tc_exp (bind_id x (tc_exp env e1) env) e2
  | ESeq (_, e1, e2) -> begin match tc_exp env e1 with
      TBot -> (* e1 will not return; no need to typecheck e2 *)
        TBot
      | _ -> tc_exp env e2
  end
  (* TODO: well-formedness explodes if EEmptyArray does not have an annotation;
     relax this behavior. *)
  | ERef (p1, RefCell, EEmptyArray (p2, elt_typ)) -> 
    let elt_typ = check_kind p2 env elt_typ in
    mk_array_typ p2 env elt_typ
  | ERef (p1, RefCell, EArray (p2, [])) -> 
    raise (Typ_error (p2, "an empty array literal requires a type annotation"))
  | ERef (p1, RefCell, EArray (p2, es)) ->
    begin match map (tc_exp env) es with
      | t1::ts ->
        let tarr = List.fold_right (typ_union env) ts t1 in
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
      | t -> raise (Typ_error (p, "cannot read an expression of type " ^
        (string_of_typ t)))
  end 
  | ESetRef (p, e1, e2) -> 
    begin match (expose_simpl_typ env (tc_exp env e1)), tc_exp env e2 with
      | TRef s, t
      | TSink s, t ->
        if not (subtype env t s) then
          typ_mismatch p
            (sprintf "left-hand side of assignment has type %s, but the \
                  right-hand side has type %s"
               (string_of_typ s) (string_of_typ t));
        t
      | s, t -> 
        typ_mismatch p
          (sprintf "left-hand side of assignment has type %s"
             (string_of_typ s));
        t
    end
  | ELabel (p, l, t, e) -> 
    let t = check_kind p env t in
    let s = tc_exp (bind_lbl l t env) e in
    if subtype env s t then t
    else raise (Typ_error (p, "label type mismatch")) (* should never happen *)
  | EBreak (p, l, e) ->
    let s = 
      try lookup_lbl l env
      with Not_found -> 
        raise (Typ_error (p, "label " ^ l ^ " is not defined")) (* severe *)
    and t = tc_exp env e in
    if not (subtype env t s) then
      typ_mismatch p
        (match l with
            "%return" -> sprintf 
              "this expression has type %s, but the function\'s return \
                     type is %s" (string_of_typ t) (string_of_typ s)
          | _ -> (* This should not happen. Breaks to labels always have 
                    type typ_undef *)
            sprintf "this expression has type %s, but the label %s has \
                          type %s" (string_of_typ t) l (string_of_typ s));
		TBot
  | ETryCatch (_, e1, x, e2) ->
    let t1 = tc_exp env e1
    and t2 = tc_exp (bind_id x TTop env) e2 in
    typ_union env t1 t2
  | ETryFinally (_, e1, e2) -> 
    let _ = tc_exp env e1 in
    tc_exp env e2
  | EThrow (_, e) -> 
    let _ = tc_exp env e in
    TBot
  | ETypecast (p, rt, e) -> 
    let t = tc_exp env e in
    static env rt t
  | EIf (p, e1, e2, e3) ->
    let c = tc_exp env e1 in
    if not (subtype env c typ_bool) then
      typ_mismatch p
        ("expected condition to have type Bool, but got a " ^ 
            (string_of_typ c));
    typ_union env (tc_exp env e2) (tc_exp env e3)
  | EObject (p, fields) ->
    let mk_field (name, exp) = 
	    (P.singleton name, PPresent (tc_exp env exp)) in
    let get_names (name, _) names = 
      P.union names (P.singleton name) in
    let rest = List.fold_right get_names fields (P.singleton "__proto__") in
    let rest' = P.negate rest in
      (* TODO: everything else hsould be absent *)
    if List.mem "__proto__" (map fst fields) then
      TObject (mk_obj_typ ((rest', PAbsent)::(map mk_field fields)))
    else
      TObject (mk_obj_typ
		             ((rest', PAbsent)::(P.singleton "__proto__",
				                             PPresent (TId "Object")) 
	                :: (map mk_field fields)))
  | EBracket (p, obj, field) -> 
    begin match simpl_typ env (tc_exp env field) with
      | TRegex pat -> inherits p env (un_null (tc_exp env obj)) pat
      | TId x -> begin match expose env (TId x) with
	        | TRegex _ -> 
	          inherits p env (un_null (tc_exp env obj)) (P.var x)
	        | t ->
	          error p (sprintf "index variable %s, is a subtype of %s"
		                   x (string_of_typ t))
      end
      | idx_typ -> error p (sprintf "index has type %s" (string_of_typ idx_typ))
    end
  | EUpdate (p, obj, field, value) -> begin
    let tobj = tc_exp env obj in
    match expose_simpl_typ env (tc_exp env obj), 
      expose_simpl_typ env (tc_exp env field), tc_exp env value with
        | TObject o, (TRegex idx_pat as tfld), typ ->
	        let fs : field list = fields o in
          let okfield (fld_pat, prop) = 
            if P.is_overlapped fld_pat idx_pat
            then match prop with
		          | PInherited s
              | PPresent s
              | PMaybe s -> 
                if not (subtype env typ s) then
                  typ_mismatch p
                    (sprintf "%s not subtype of %s in %s[%s = %s]"
                       (string_of_typ typ) 
                       (string_of_typ s) 
                       (string_of_typ tobj)
                       (string_of_typ tfld)
                       (string_of_typ typ))
              | PAbsent -> 
                typ_mismatch p (sprintf "Assigning to absent field") in
          let _ = List.iter okfield fs in
          tobj
        | obj, fld, typ ->
          let _ = typ_mismatch p (sprintf "Bad update: %s[%s = %s]"
                                    (string_of_typ obj) 
                                    (string_of_typ fld) 
                                    (string_of_typ typ)) in
          obj
          
  end
  | EPrefixOp (p, op, e) -> tc_exp env (EApp (p, EId (p, op), [e]))
  | EInfixOp (p, "+", e1, e2) -> 
    begin match (tc_exp env e1, tc_exp env e2) with
      | TRegex _, _
      | _, TRegex _ -> 
	      TRegex P.all
      | t1, t2 ->
	      tc_exp env (EApp (p, EId (p, "+"), [ ECheat (p, t1, e1); 
					                                   ECheat (p, t2, e2) ]))
    end
  | EInfixOp (p, op, e1, e2) -> tc_exp env (EApp (p, EId (p, op), [e1; e2]))
  | EApp (p, f, args) -> 
    let arg_typs = map (tc_exp_ret env) args in
    let assumed_arg_exps = 
      List.map2 (fun e t -> ECheat (p, t, e)) args arg_typs in
    let rec check_app tfun =
      begin match expose_simpl_typ env tfun with 
        | TArrow (expected_typs, result_typ) ->
					(* TODO: allow arguments to be elided *)
          let arg_typs = fill (List.length expected_typs - List.length args) 
            (TPrim Undef) arg_typs in
					let check_arg s t =
						try assert_subtyp env p s t 
						with Typ_error (p, msg) -> 
							typ_mismatch p 
								(sprintf "expected argument of type %s, received %s"
									 (string_of_typ s) (string_of_typ t)) in
					begin
            try List.iter2 check_arg arg_typs expected_typs
						with Invalid_argument "List.iter2" -> 
							typ_mismatch p
								(sprintf "arity-mismatch: the function expects %d \
                                  arguments, but %d arguments given"
									 (List.length expected_typs) (List.length args))
					end;
					result_typ
        | TIntersect (t1, t2) -> 
					begin 
						try 
							let r1 = check_app t1 in 
							begin 
								try
                  let r2 = check_app t2 in
                  typ_intersect env r1 r2
                with
									| Not_subtype _ 
									| Typ_error _ -> r1 
							end
            with
							| Not_subtype _ 
							| Typ_error _ -> check_app t2 
					end
				| (TForall _) as quant_typ -> 
					begin match Typ.forall_arrow quant_typ with
						| None -> 
							error p (sprintf "expected function, got %s"
												 (string_of_typ quant_typ))
						| Some (typ_vars, (TArrow (_, r) as arrow_typ)) ->
							let assoc =
								typ_assoc env arrow_typ (TArrow (arg_typs, r)) in
							let guess_typ_app exp typ_var = 
								try
									let guessed_typ = IdMap.find typ_var assoc in
									ETypApp (p, exp, guessed_typ) 
								with Not_found -> begin
									error p (sprintf "$$$ could not instantiate") end in
							let guessed_exp = 
								fold_left guess_typ_app (ECheat (p, quant_typ, f)) 
									typ_vars in
							tc_exp env (EApp (p, guessed_exp, assumed_arg_exps))
						| Some _ -> failwith "expected TArrow from forall_arrow"
					end
				| not_func_typ ->
					error p (sprintf "expected function, got %s" 
										 (string_of_typ not_func_typ))
      end in check_app (un_null (tc_exp env f)) 
  | ERec (binds, body) -> 
    (* No kinding check here, but it simply copies the type from the function.
       Let it work. (Actual reason: no position available) *)
    let f env (x, t, e) = bind_id x t env in
    let env = fold_left f env binds in
    let tc_bind (x, t, e) =
      let s = tc_exp env e in
      if subtype env s t then ()
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
    let expected_typ = check_kind p env (func_info.func_typ) in
    begin match bind_typ env (expose_simpl_typ env expected_typ) with
      | (env, TArrow (arg_typs, result_typ)) ->
        if not (List.length arg_typs = List.length args) then
          error p 
            (sprintf "given %d argument names, but %d argument types"
               (List.length args) (List.length arg_typs));
        let bind_arg env x t = bind_id x t env in
        let env = List.fold_left2 bind_arg env args arg_typs in
        let env = clear_labels env in
        let body = 
          if !is_flows_enabled then
            Sb_semicfa.semicfa (func_info.func_owned) env body 
          else 
            body in
        let body_typ = tc_exp env body in
        if not (subtype env body_typ result_typ) then 
          typ_mismatch p
            (sprintf "function body has type\n%s\n, but the \
                      return type is\n%s" 
               (string_of_typ body_typ)
               (string_of_typ result_typ));
        expected_typ
      | _ -> raise (Typ_error (p, "invalid type annotation on a function"))
    end
  | ESubsumption (p, t, e) ->
    let t = check_kind p env t in
    let s = tc_exp env e in
    if not (subtype env s t) then
      typ_mismatch p
        (sprintf "invalid upcast: %s is not a subtype of %s"
	         (string_of_typ s) (string_of_typ t));
    t
  | EAssertTyp (p, t, e) ->
    let t = check_kind p env t in
    let s = tc_exp env e in
    if subtype env s t then
      s (* we do not subsume *)
    else
      raise 
        (Typ_error (p,  sprintf
          "expression has type %s, which is incompatible with the \
                      annotation" (string_of_typ s)))
  | EDowncast (p, t, e) -> 
    let t = check_kind p env t in
    let (p1, p2) = Exp.pos e in 
    contracts := IntMap.add p1.Lexing.pos_cnum (p2.Lexing.pos_cnum, t)
      !contracts;
    ignore (tc_exp env e);
    t
  | ETypAbs (p, x, t, e) ->
    (* bind_typ_id does the kinding check *)
    let env = bind_typ_id x t env in
    TForall (x, t, tc_exp env e)
  | ETypApp (p, e, u) ->
    begin match expose_simpl_typ env (tc_exp env e) with
      | TForall (x, s, t) ->
        if subtype env u s then
          typ_subst x u t
        else 
          error p (sprintf "expected an argument of type \n %s, got \n %s"
                     (string_of_typ s) (string_of_typ u))
      | t ->
        error p (sprintf "expected forall-type in type application, got:\
                                \n%s\nargument has type:\n%s"
		               (string_of_typ t) (string_of_typ u))
    end
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
    simpl_typ env t

let typecheck env exp =
  let _ = tc_exp env exp in
  ()

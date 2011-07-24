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

let rec check (env : env) (exp : exp) (typ : typ) : unit = match exp with
  | ELabel (p, lbl, e) ->
    let s = tc_exp (bind_lbl lbl typ env) e in
    if not (subtype env s typ) then
      raise (Typ_error (p, "label type mismatch")) (* should never happen *)
  | ELet (_, x, e1, e2) -> 
    check (bind_id x (tc_exp env e1) env) e2 typ
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
    let expected_typ = check_kind p env typ in
    begin match bind_typ env (expose_simpl_typ env expected_typ) with
      | (env, TArrow (arg_typs, result_typ)) ->
        if not (List.length arg_typs = List.length args) then
          typ_mismatch p
            (sprintf "given %d argument names, but %d argument types"
               (List.length args)
               (List.length arg_typs))
        else
          let bind_arg env x t = bind_id x t env in
          let env = List.fold_left2 bind_arg env args arg_typs in
          let env = clear_labels env in
          let body = 
            if !is_flows_enabled then
              Sb_semicfa.semicfa (func_info.func_owned) env body 
            else 
              body in
          check env body result_typ
      | _, t -> 
        raise (Typ_error (p, sprintf "invalid type annotation on a function: %s"
          (string_of_typ t)))
    end
  | _ -> 
    let synth_typ = tc_exp env exp in
    if not (subtype env synth_typ typ) then
      typ_mismatch (Exp.pos exp)
        (sprintf "expected %s, got %s" 
           (string_of_typ typ)
           (string_of_typ synth_typ))

and tc_exp (env : env) (exp : exp) : typ = match exp with
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
	        let fld_typ = simpl_lookup p (tid_env env) (TObject ot) pat in
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
    let t = expose_simpl_typ env (tc_exp env e1) in
    begin match  t with
      | TRef s 
      | TSink s -> check env e2 s; t
      | s -> typ_mismatch p
        (sprintf "left-hand side of assignment has type %s"
           (string_of_typ s));
        t
    end
  | ELabel (p, t, e) -> 
    raise (Typ_error (p, "cannot calculate type for label"))
  | EBreak (p, lbl, e) ->
    let lbl_typ = 
      try lookup_lbl lbl env
      with Not_found -> (* severe *)
        raise (Typ_error (p, "label " ^ lbl ^ " is not defined")) in
    check env e lbl_typ;
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
    check env e1 typ_bool;
    typ_union env (tc_exp env e2) (tc_exp env e3)
  | EObject (p, fields) ->
    let mk_field (name, exp) = 
	    (P.singleton name, PPresent (tc_exp env exp)) in
    let get_names (name, _) names = 
      P.union names (P.singleton name) in
    let rest = List.fold_right get_names fields (P.singleton "__proto__") in
    let rest' = P.negate rest in
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
	          raise (Typ_error 
                     (p, 
                      sprintf "index variable %s, is a subtype of %s"
		                    x (string_of_typ t)))
      end
      | idx_typ -> 
        raise (Typ_error
                 (p, sprintf "index has type %s" (string_of_typ idx_typ)))
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
    let rec check_app tfun =
      begin match expose_simpl_typ env tfun with 
        | TArrow (expected_typs, result_typ) ->
          let args = fill (List.length expected_typs - List.length args) 
            (EConst (p, JavaScript_syntax.CUndefined)) args in
					begin
            try List.iter2 (check env) args expected_typs
						with Invalid_argument "List.iter2" -> 
              typ_mismatch p
                (sprintf "arity-mismatch:  %d args expected, but %d given"
									 (List.length expected_typs) (List.length args))
					end;
					result_typ
        | TIntersect (t1, t2) -> 
          with_typ_exns
            (fun () ->
						  try 
							  let r1 = check_app t1 in 
							  begin 
								  try
                    let r2 = check_app t2 in
                    typ_intersect env r1 r2
                  with | Typ_error _ -> r1
							  end
              with | Typ_error _ -> check_app t2)
				| (TForall _) as quant_typ -> 
					begin match Typ.forall_arrow quant_typ with
						| None -> 
							raise (Typ_error (p, sprintf "expected function, got %s"
								(string_of_typ quant_typ)))
						| Some (typ_vars, (TArrow (_, r) as arrow_typ)) ->
              (* guess-work breaks bidirectionality *)
              let arg_typs = map (tc_exp_ret env) args in
              let assumed_arg_exps = 
                List.map2 (fun e t -> ECheat (p, t, e)) args arg_typs in
							let assoc =
								typ_assoc env arrow_typ (TArrow (arg_typs, r)) in
							let guess_typ_app exp typ_var = 
								try
									let guessed_typ = IdMap.find typ_var assoc in
									ETypApp (p, exp, guessed_typ) 
								with Not_found -> begin
									raise (Typ_error (p, "$$$ could not instantiate")) end in
							let guessed_exp = 
								fold_left guess_typ_app (ECheat (p, quant_typ, f)) 
									typ_vars in
							tc_exp env (EApp (p, guessed_exp, assumed_arg_exps))
						| Some _ -> failwith "expected TArrow from forall_arrow"
					end
				| not_func_typ ->
          (* even in an intersection, this should count as a genuine error *)
          raise (Typ_error (p,
                            sprintf "expected function, got %s" 
							                (string_of_typ not_func_typ)))
      end in 
    check_app (un_null (tc_exp env f))
  | ERec (binds, body) -> 
    (* No kinding check here, but it simply copies the type from the function.
       Let it work. (Actual reason: no position available) *)
    let f env (x, t, e) = bind_id x t env in
    let env = fold_left f env binds in
    let tc_bind (x, t, e) = check env e t in
    List.iter tc_bind binds;
    tc_exp env body
  | EFunc (p, args, func_info, body) -> 
    raise (Typ_error (p, "cannot calculate function type; annotation required"))
  | ESubsumption (p, t, e) ->
    let t = check_kind p env t in
    check env e t;
    t
  | EAssertTyp (p, t, e) ->
    let t = check_kind p env t in
    let _ = check env e t in
    t
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
          begin
            typ_mismatch p
              (sprintf "type-argument %s is not a subtype of the bound %s"
                 (string_of_typ u) (string_of_typ s));
            typ_subst x s t (* Warning: produces possibily spurious errors *)
          end
      | t ->
        raise
          (Typ_error (p, sprintf "expected forall-type in type application, \
                                  got:\n%s\nargument has type:\n%s"
		        (string_of_typ t) (string_of_typ u)))
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
  let _ = if !error_on_unreachable && t = TBot then
      typ_mismatch (Exp.pos e) "unreachable code" in
  simpl_typ env t

let typecheck env exp =
  let _ = tc_exp env exp in
  ()

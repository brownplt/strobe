open Prelude
open Typedjs_syntax

module P = Sb_strPat

(** Decides if two types are syntactically equal. This helps subtyping. *)
let rec simpl_equiv (typ1 : typ) (typ2 : typ) : bool =
  match (typ1, typ2) with
    | TTop, TTop
    | TBot, TBot ->
      true
    | TPrim p1, TPrim p2 ->
      p1 = p2
    | TIntersect (s1, s2), TIntersect (t1, t2)
    | TUnion (s1, s2), TUnion (t1, t2) -> 
      simpl_equiv s1 t1 && simpl_equiv s2 t2
    | TSource s, TSource t
    | TSink s, TSink t
    | TRef s, TRef t ->
      simpl_equiv s t
    | TApp (s1, s2s), TApp (t1, t2s) ->
      (* for well-kinded types, for_all2 should not signal an error *)
      simpl_equiv s1 t1 && List.for_all2 simpl_equiv s2s t2s
    | TId x, TId y ->
      x = y
    | TArrow (args1, r1), TArrow (args2, r2) ->
      List.length args1 = List.length args2
      && List.for_all2 simpl_equiv args1 args2
      && simpl_equiv r1 r2
    | TRec (x, s), TRec (y, t) ->
      x = y && simpl_equiv s t
    | TForall (x, s1, s2), TForall (y, t1, t2) ->
      x = y && simpl_equiv s1 t1 && simpl_equiv s2 t2
    | TRegex pat1, TRegex pat2 ->
      P.is_equal pat1 pat2
    | TObject o1, TObject o2 ->
      let flds1 = fields o1 in
      let flds2 = fields o2 in
      List.length flds1 = List.length flds2
      && List.for_all2 simpl_equiv_fld flds1 flds2
    | TFix (x1, k1, t1), TFix (x2, k2, t2) ->
      x1 = x2 && k1 = k2 && simpl_equiv t1 t2
    | TLambda (args1, t1), TLambda (args2, t2) ->
      args1 = args2 && simpl_equiv t1 t2
    | _, _ -> false

and simpl_equiv_fld (pat1, fld1) (pat2, fld2) = 
  P.is_equal pat1 pat2 &&
    begin match (fld1, fld2) with
      | PPresent t1, PPresent t2
      | PInherited t1, PInherited t2
      | PMaybe t1, PMaybe t2 ->
	simpl_equiv t1 t2
      | PAbsent, PAbsent -> true
      | _ -> false
    end

exception Kind_error of string

type kind_env = kind IdMap.t

let kind_mismatch typ calculated_kind expected_kind = 
  raise 
    (Kind_error 
       (sprintf "Expected kind %s, but got kind %s for type:\n%s"
	  (string_of_kind expected_kind)
	  (string_of_kind calculated_kind)
	  (string_of_typ typ)))


let rec kind_check (env : kind_env) (typ : typ) : kind = match typ with
  | TTop
  | TBot
  | TRegex _
  | TPrim _ -> KStar
  | TUnion (t1, t2)
  | TIntersect (t1, t2) ->
    begin match kind_check env t1, kind_check env t2 with
      | KStar, KStar -> KStar
      | k1, KStar -> kind_mismatch t1 k1 KStar
      | _, k2 -> kind_mismatch t2 k2 KStar
    end
  | TRef t
  | TSource t
  | TSink t ->
    begin match kind_check env t with
      | KStar -> KStar
      | k -> kind_mismatch t k KStar
    end
  | TArrow (arg_typs, result_typ) ->
    let assert_kind t = match kind_check env t with
      | KStar -> ()
      | k -> kind_mismatch t k KStar in
    List.iter assert_kind (result_typ :: arg_typs);
    KStar
  | TObject o ->
    List.iter (assert_fld_kind env) (fields o);
    KStar
  | TId x -> 
    begin 
      try IdMap.find x env
      with Not_found ->
	raise (Kind_error (sprintf "type variable %s is unbound" x))
    end
  | TForall (x, t1, t2) ->
    begin match kind_check env t1, kind_check (IdMap.add x KStar env) t2 with
      | KStar, KStar -> KStar
      | k1, KStar -> kind_mismatch t1 k1 KStar
      | _, k2 -> kind_mismatch t2 k2 KStar
    end
  | TRec (x, t) ->
    begin match kind_check (IdMap.add x KStar env) t with
      | KStar -> KStar
      | k -> kind_mismatch t k KStar
    end
  | TLambda (args, t) ->
    let env' = fold_right (fun (x, k) env -> IdMap.add x k env) args env in
    KArrow (List.map snd2 args, kind_check env' t)
  | TFix (x, k, t) ->
    let k' = kind_check (IdMap.add x k env) t in
    if  k' = k then k
    else kind_mismatch typ k' k
  | TApp (t_op, t_args) ->
    begin match kind_check env t_op with
      | KArrow (k_args, k_result) ->
	begin
	  try
	    let check k_arg t_arg = 
	      let k_actual = kind_check env t_arg in
	      if k_arg = k_actual then
		()
	      else 
		kind_mismatch t_arg k_actual k_arg in
	    let _ = List.iter2 check k_args t_args in
	    k_result
	  with Invalid_argument _ ->
	    raise (Kind_error
		     (sprintf "operator expects %d args, given %d"
			(List.length k_args) (List.length t_args)))
	end
      | KStar ->
	raise (Kind_error 
		 (sprintf "not a type operator:\n%s" (string_of_typ t_op)))
    end

and assert_fld_kind (env : kind_env) (_, prop) = match prop with
  | PPresent t
  | PInherited t
  | PMaybe t ->
    begin match kind_check env t with
      | KStar -> ()
      | k -> kind_mismatch t k KStar
    end
  | PAbsent ->
    ()

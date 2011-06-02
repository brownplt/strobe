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
    | TApp (s1, s2), TApp (t1, t2)
    | TIntersect (s1, s2), TIntersect (t1, t2)
    | TUnion (s1, s2), TUnion (t1, t2) -> 
      simpl_equiv s1 t1 && simpl_equiv s2 t2
    | TSource s, TSource t
    | TSink s, TSink t
    | TRef s, TRef t ->
      simpl_equiv s t
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
    | TObject flds1, TObject flds2 ->
      List.length flds1 = List.length flds2
      && List.for_all2 simpl_equiv_fld flds1 flds2
    | TLambda (x1, k1, t1), TFix (x2, k2, t2)
    | TFix (x1, k1, t1), TFix (x2, k2, t2) ->
      x1 = x2 && k1 = k2 && simpl_equiv t1 t2
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


let rec kind_check (env : kind_env) (syns : kind_env) (typ : typ) : kind = match typ with
  | TTop
  | TBot
  | TRegex _
  | TPrim _ -> KStar
  | TUnion (t1, t2)
  | TIntersect (t1, t2) ->
    begin match kind_check env syns t1, kind_check env syns t2 with
      | KStar, KStar -> KStar
      | k1, KStar -> kind_mismatch t1 k1 KStar
      | _, k2 -> kind_mismatch t2 k2 KStar
    end
  | TRef t
  | TSource t
  | TSink t ->
    begin match kind_check env syns t with
      | KStar -> KStar
      | k -> kind_mismatch t k KStar
    end
  | TArrow (arg_typs, result_typ) ->
    let assert_kind t = match kind_check env syns t with
      | KStar -> ()
      | k -> kind_mismatch t k KStar in
    List.iter assert_kind (result_typ :: arg_typs);
    KStar
  | TObject flds ->
    List.iter (assert_fld_kind env syns) flds;
    KStar
  | TId x -> 
    begin 
      try IdMap.find x env
      with Not_found ->
	raise (Kind_error (sprintf "type variable %s is unbound" x))
    end
  | TSyn x ->
    begin 
      try IdMap.find x syns
      with Not_found ->
        raise (Kind_error (sprintf "type synonym %s is unbound" x))
    end
  | TForall (x, t1, t2) ->
    begin match kind_check env syns t1, kind_check (IdMap.add x KStar env) syns t2 with
      | KStar, KStar -> KStar
      | k1, KStar -> kind_mismatch t1 k1 KStar
      | _, k2 -> kind_mismatch t2 k2 KStar
    end
  | TRec (x, t) ->
    begin match kind_check (IdMap.add x KStar env) syns t with
      | KStar -> KStar
      | k -> kind_mismatch t k KStar
    end
  | TLambda (x, k, t) ->
    KArrow (k, kind_check (IdMap.add x k env) syns t)
  | TFix (x, k, t) ->
    let k' = kind_check (IdMap.add x k env) syns t in
    if  k' = k then k
    else kind_mismatch typ k' k
  | TApp (t1, t2) ->
    begin match kind_check env syns t1, kind_check env syns t2 with
      | KArrow (k_expected, k_result), k_arg ->
	if k_expected = k_arg then
	  k_result
	else
	  kind_mismatch t2 k_arg k_expected
      | KStar, _ ->
	raise (Kind_error 
		 (sprintf "not a type operator:\n%s" (string_of_typ t1)))
    end

and assert_fld_kind (env : kind_env) (syns: kind_env) (_, prop) = match prop with
  | PPresent t
  | PInherited t
  | PMaybe t ->
    begin match kind_check env syns t with
      | KStar -> ()
      | k -> kind_mismatch t k KStar
    end
  | PAbsent ->
    ()

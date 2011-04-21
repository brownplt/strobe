open Prelude
open Typedjs_syntax

module P = Sb_strPat

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
    | _, _ -> false

and simpl_equiv_fld (pat1, fld1) (pat2, fld2) = 
  P.is_equal pat1 pat2 &&
    begin match (fld1, fld2) with
      | PPresent t1, PPresent t2
      | PMaybe t1, PMaybe t2 ->
	simpl_equiv t1 t2
      | PAbsent, PAbsent -> true
    end
      

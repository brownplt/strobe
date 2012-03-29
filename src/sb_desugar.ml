open Prelude
open Typedjs_syntax
open TypImpl

module W = WritTyp
module List = ListExt

exception Typ_stx_error of string
let error msg = 
  raise (Typ_stx_error msg)

let is_star f = match f with
  | W.Star _ -> true
  | _ -> false

let is_skull f = match f with
  | W.Skull _ -> true
  | _ -> false

let is_absent f = match f with
  | W.Absent p -> true
  | _ -> false

let pat_of f = match f with
  | W.Present (p, _) -> p
  | W.Inherited (p, _) -> p
  | W.Maybe (p, _) -> p
  | W.Absent p -> p
  | W.Skull p -> p
  | W.Star _ -> failwith "pat_of applied to Star _"

let assert_overlap pat1 pat2 = match P.example (P.intersect pat1 pat2) with
  | None -> ()
  | Some str ->
    error (sprintf "%s and %s are overlapped. E.g.,\n%s\n is in both patterns." 
       (P.pretty pat1) (P.pretty pat2) str)

let rec typ (writ_typ : W.t) : typ =
  let opt_map f v = match v with None -> None | Some v -> Some (f v) in
  match writ_typ with
  | W.Str -> TRegex P.all
  | W.Prim p -> TPrim p
  | W.Bool -> TUnion (Some "Bool", TPrim "True", TPrim "False")
  | W.Union (t1, t2) -> TUnion (None, typ t1, typ t2)
  | W.Inter (t1, t2) -> TIntersect (None, typ t1, typ t2)
  | W.Arrow (None, args, var, r) -> TArrow (map typ args, opt_map typ var, typ r)
  | W.Arrow (Some this, args, var, r) -> TArrow ((typ this):: (map typ args), opt_map typ var, typ r)
  | W.This t -> TThis (typ t)
  | W.Object flds -> object_typ flds
  | W.With(t, flds) -> (match object_typ flds with 
    | TObject objflds -> TWith(typ t, objflds)
    | _ -> failwith "absurd")
  | W.Pat pat -> TRegex pat
  | W.Ref t -> TRef (None, typ t)
  | W.Source t -> TSource (None, typ t)
  | W.Top -> TTop
  | W.Bot -> TBot
  | W.Syn x -> TId x
  | W.Id x -> TId x
  | W.App (t1, t2s) -> TApp (typ t1, map typ t2s)
  | W.Forall (x, s, t) -> TForall (None, x, typ s, typ t)
  | W.Rec (x, t) -> TRec (None, x, typ t)
  | W.Lambda (args, t) -> TLambda (None, args, typ t)
  | W.Fix (x, k, t) -> TFix (None, x, k, typ t)

and fld (writ_fld : W.f) : field = match writ_fld with
  | W.Present (pat, t) -> (pat, Present, typ t)
  | W.Maybe (pat, t) -> (pat, Maybe, typ t)
  | W.Inherited (pat, t) -> (pat, Inherited, typ t)
  | W.Absent pat -> error "fld applied to Absent"
  | W.Skull _ -> error "fld applied to Skull"
  | W.Star _ -> error "fld applied to Star"

and object_typ (flds : W.f list) =
  let (flds_no_absents, absent_pat) = 
    let (absents, others) = List.partition is_absent flds in
    (others, 
     fold_right (fun w acc -> P.union (pat_of w) acc) absents P.empty) in
  let (flds_no_stars, absent_pat) =
    let (stars, others) = List.partition is_star flds_no_absents in
    match stars with
    | [] -> let skulls = List.filter is_skull others in
            begin match skulls with
            | [] -> (others, absent_pat)
            | _ -> error "BAD is nonsensical without *"
            end
    | [W.Star opt_star_typ] ->
      let star_pat =
        P.negate (fold_right P.union (map pat_of others) absent_pat) in
      begin match opt_star_typ with
      | None -> (others, P.union star_pat absent_pat)
      | Some t -> ((W.Maybe (star_pat, t)) :: others, absent_pat)
      end
   | _ -> error "multiple stars (*) in an object type" in
  (* TODO(arjun): Why is this overlap check here? Can we do it at the top
     of the function? *)
  List.iter_pairs assert_overlap 
    (absent_pat :: (List.map pat_of flds_no_stars));
  let flds_no_skulls_stars = 
    List.filter (fun f -> not (is_skull f)) flds_no_stars in
  TObject (mk_obj_typ (map fld flds_no_skulls_stars) absent_pat)



let desugar_typ (p : pos) (wt : W.t) : typ =
  try typ wt
  with Typ_stx_error msg ->
    raise (Typ_stx_error (string_of_position p ^ msg))

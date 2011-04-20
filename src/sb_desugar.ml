open Prelude
open Typedjs_syntax

module W = WritTyp
module P = Sb_strPat

module List = struct
  include List

  let rec tails (lst : 'a list) : 'a list list = match lst with
    | [] -> [ [] ]
    | _ :: lst' -> lst :: (tails lst')

  let iter_pairs (f : 'a -> 'a -> unit) (lst : 'a list) : unit =
    let g lst = match lst with
      | x :: rest -> iter (f x) rest
      | _ -> () in
    iter g (tails lst)

end

exception Typ_stx_error of string
let error msg = 
  raise (Typ_stx_error msg)

let is_star f = match f with
  | W.Star _ -> true
  | _ -> false

let is_skull f = match f with
  | W.Skull _ -> true
  | _ -> false

let pat_of f = match f with
  | W.Present (p, _) -> p
  | W.Maybe (p, _) -> p
  | W.Absent p -> p
  | W.Skull p -> p
  | W.Star _ -> failwith "pat_of applied to Star _"

let assert_overlap pat1 pat2 = match P.example (P.intersect pat1 pat2) with
  | None -> ()
  | Some str ->
    error (sprintf "%s and %s are overlapped. E.g.,\n%s\n is in both patterns." 
	     (P.pretty pat1) (P.pretty pat2) str)

let rec typ (writ_typ : W.t) : typ = match writ_typ with
  | W.Str -> TRegex Sb_strPat.all
  | W.Prim p -> TPrim p
  | W.Bool -> TUnion (TPrim True, TPrim False)
  | W.Union (t1, t2) -> TUnion (typ t1, typ t2)
  | W.Inter (t1, t2) -> TIntersect (typ t1, typ t2)
  | W.Arrow (None, args, r) -> TArrow (map typ args, typ r)
  | W.Arrow (Some this, args, r) -> TArrow ((typ this):: (map typ args), typ r)
  | W.Object flds -> object_typ flds
  | W.Pat pat -> TRegex pat
  | W.Ref t -> TRef (typ t)
  | W.Source t -> TRef (typ t)
  | W.Top -> TTop
  | W.Bot -> TBot
  | W.Syn x -> TId x
  | W.Id x -> TId x
  | W.App (t1, t2) -> TApp (typ t1, typ t2)
  | W.Forall (x, s, t) -> TForall (x, typ s, typ t)
  | W.Lambda (x, t) -> TLambda (x, KStar, typ t)
  | W.Rec (x, t) -> TRec (x, typ t)

and fld (writ_fld : W.f) : pat * prop = match writ_fld with
  | W.Present (pat, t) -> (pat, PPresent (typ t))
  | W.Maybe (pat, t) -> (pat, PMaybe (typ t))
  | W.Absent pat -> (pat, PAbsent)
  | W.Skull _ -> error "fld applied to Skull"
  | W.Star _ -> error "fld applied to Star"

and object_typ (lst : W.f list) =
  let flds = (* Present, Maybe, Absent, and Skull only *)
    let (stars, others) = List.partition is_star lst in
    match stars with
      | [] -> let skulls = List.filter is_skull others in
	      begin match skulls with
		| [] -> others
		| _ -> error "BAD is nonsensical without *"
	      end
      | [W.Star opt_star_typ] ->
	let star_pat = 
	  P.negate (fold_right P.union (map pat_of others) P.empty) in
	begin match opt_star_typ with
	  | None -> (W.Absent star_pat) :: others
	  | Some t -> (W.Maybe (star_pat, t)) :: others
	end
      | _ -> error "multiple stars (*) in an object type" in
  List.iter_pairs assert_overlap (List.map pat_of flds);
  let flds = List.filter (fun f -> not (is_skull f)) flds in
  let (proto, non_proto_flds) = 
    List.partition
      (fun fld -> match P.singleton_string (pat_of fld) with
	| Some "proto" -> true
	| _ -> false)
      flds in
  match proto with
    | [W.Present (_, proto_typ)] ->
      TObject (map fld non_proto_flds, typ proto_typ)
    | [] -> TObject (map fld non_proto_flds, TId "Object")
    | [_] -> error "proto must be definitely present"

    | _ -> error "multiple proto fields found"

let desugar_typ (p : pos) (wt : W.t) : typ =
  try typ wt
  with Typ_stx_error msg ->
    raise (Typ_stx_error (string_of_position p ^ msg))

open Prelude
open Typedjs_syntax

(** Some of these constructor names correspond directly to the object names
    in JavaScript. Good or bad? *)

let typ_str = TConstr ("String", [])

let typ_regexp = TConstr ("RegExp", [])

let typ_num = TConstr ("Number", [])

let typ_int = TConstr ("Int", [])

let typ_bool = TConstr ("Boolean", [])

let typ_null = TConstr ("Null", [])

let typ_undef = TConstr ("Undefined", [])

(* assumes s and t are in normal form *)
let rec subtype (classes : typ IdMap.t) (s : typ) (t : typ) : bool = 
  let subtype = subtype classes
  and subtypes = subtypes classes in
    match s, t with
      | TConstr ("Int", []), TConstr ("Number", []) -> true
(*
      | TConstr ("Number", []), TConstr ("String", []) -> true 
      | TConstr ("Int", []), TConstr ("String", []) -> true *)
      | TConstr (c1, args1), TConstr (c2, args2) ->
          if c1 = c2 then subtypes args1 args2 else false
      | TUnion (s1, s2), _ -> 
          subtype s1 t && subtype s2 t
      | _, TUnion (t1, t2) ->
          subtype s t1 || subtype s t2
      | TArrow (_, args1, r1), TArrow (_, args2, r2) ->
          subtypes args2 args1 && subtype r1 r2
      | TObject fs1, TObject fs2 -> subtype_fields classes fs1 fs2
          (* objects vs. constructed objects: *)
      | TObject fs1, TConstr (cid, apps) -> begin try
          match IdMap.find cid classes with
              TObject fs2 -> subtype_fields classes fs1 fs2
        with
            _ -> false
        end
      | TConstr (cid, apps), TObject fs2 -> begin try
          match IdMap.find cid classes with
              TObject fs1 -> subtype_fields classes fs1  fs2
        with
            _ -> false
        end
          (* this will handle inheritance as well: *)
      | TConstr (cid1, apps1), TConstr (cid2, apps2) -> begin try
          match IdMap.find cid1 classes, IdMap.find cid2 classes with
              TObject fs1, TObject fs2 -> subtype_fields classes fs1 fs2
        with
            _ -> false
        end
      | TRef s', TRef t' -> subtype s' t' && subtype t' s'
      | TSource s, TSource t -> subtype s t
      | TSink s, TSink t -> subtype t s
      | TRef s, TSource t -> subtype s t
      | TRef s, TSink t -> subtype t s
      | _, TTop ->
          true
      | TBot, _ ->
          true
      | _ ->
          s = t
      
(* assumes fs1 and fs2 are ordered *)
and subtype_fields classes fs1 fs2 = match fs1, fs2 with
  | [], [] -> true
  | [], _ -> true
  | _, [] -> false (* fs1 has fields that fs2 does not *)
  | (x, s) :: fs1', (y, t) :: fs2' ->
      let cmp = String.compare x y in
        if cmp = 0 then subtype classes s t && subtype_fields classes fs1' fs2'
        else if cmp < 0 then false (* we will not find x in the supertype *)
          (* y is an extra field in the supertype *)
        else subtype_fields classes fs1 fs2' 

and subtypes classes (ss : typ list) (ts : typ list) : bool = 
  try List.for_all2 (subtype classes) ss ts
  with Invalid_argument _ -> false (* unequal lengths *)

let typ_union cs s t = match subtype cs s t, subtype cs t s with
    true, true -> s (* t = s *)
  | true, false -> t (* s <: t *)
  | false, true -> s (* t <: s *)
  | false, false -> TUnion (s, t)

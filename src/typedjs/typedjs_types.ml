open Prelude
open Typedjs_syntax

(** Some of these constructor names correspond directly to the object names
    in JavaScript. Good or bad? *)

let typ_str = TApp ("String", [])

let typ_regexp = TApp ("RegExp", [])

let typ_num = TApp ("Number", [])

let typ_int = TApp ("Int", [])

let typ_bool = TApp ("Boolean", [])

let typ_null = TApp ("Null", [])

let typ_undef = TApp ("Undefined", [])

let rec subtype (s : typ) (t : typ) : bool = match s, t with
    TApp ("Int", []), TApp ("Number", []) -> true
  | TApp (c1, args1), TApp (c2, args2) ->
      if c1 = c2 then subtypes args1 args2 else false
  | TUnion (s1, s2), _ -> 
      subtype s1 t && subtype s2 t
  | _, TUnion (t1, t2) ->
      subtype s t1 || subtype s t2
  | TArrow (_, args1, r1), TArrow (_, args2, r2) ->
      subtypes args2 args1 && subtype r1 r2
  | _, TTop ->
      true
  | TBot, _ ->
      true
  | _ ->
      s = t

and subtypes (ss : typ list) (ts : typ list) : bool = 
  try List.for_all2 subtype ss ts
  with Invalid_argument _ -> false (* unequal lengths *)

let typ_union s t = match subtype s t, subtype t s with
    true, true -> s (* t = s *)
  | true, false -> t (* s <: t *)
  | false, true -> s (* t <: s *)
  | false, false -> TUnion (s, t)

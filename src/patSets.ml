open Prelude

module StringSet = Set.Make (String)
module StringSetExt = SetExt.Make (StringSet)


type t =
  | Finite of StringSet.t
  | CoFinite of StringSet.t

let parse pos str =
  failwith "PatSets.parse not implemented"

let singleton str = Finite (StringSet.singleton str)

let singleton_string v = match v with
  | CoFinite _ -> None
  | Finite set -> match StringSet.cardinal set with
      | 1 -> Some (StringSet.choose set)
      | _ -> None

let empty = Finite StringSet.empty

let all = CoFinite StringSet.empty

let is_empty v = match v with
  | Finite set -> StringSet.is_empty set
  | CoFinite _ -> false

let intersect v1 v2 = 
  if (is_empty v1) then empty
  else if (is_empty v2) then empty
  else match v1, v2 with
  | Finite set1, Finite set2 -> Finite (StringSet.inter set1 set2)
  | CoFinite set1, CoFinite set2 -> CoFinite (StringSet.union set1 set2)
  | Finite fset, CoFinite cfset
  | CoFinite cfset, Finite fset -> Finite (StringSet.diff fset cfset)

let intersections ts = match (List.fold_left 
                         (fun u t -> match u with
                         | None -> Some t
                         | Some u -> Some (intersect u t))
                         None
                         ts) with
  | Some u -> u
  | None -> empty

let union v1 v2 = 
  if (is_empty v1) then v2
  else if (is_empty v2) then v1
  else match v1, v2 with
  | Finite set1, Finite set2 -> Finite (StringSet.union set1 set2)
  | CoFinite set1, CoFinite set2 -> CoFinite (StringSet.inter set1 set2)
  | Finite fset, CoFinite cfset
  | CoFinite cfset, Finite fset -> CoFinite (StringSet.diff cfset fset)

let unions ts = match (List.fold_left 
                         (fun u t -> match u with
                         | None -> Some t
                         | Some u -> Some (union u t))
                         None
                         ts) with
  | Some u -> u
  | None -> empty

let negate v = match v with
  | Finite set -> CoFinite set
  | CoFinite set -> Finite set

let subtract v1 v2 = intersect v1 (negate v2)

let concat _ _ =
  failwith "concat not implemented--probably should not be"

let is_overlapped v1 v2 = match v1, v2 with
  | Finite set1, Finite set2 ->
    not (StringSet.is_empty (StringSet.inter set1 set2))
  | CoFinite _, CoFinite _ -> 
    (* There is always some element not in the set of excluded strings that
       is common to both co-finite sets. *)
    true
  | Finite fset, CoFinite cfset
  | CoFinite cfset, Finite fset ->
    (* The finite set must be contained in the excluded elements of the
       co-finite set. Any element not explicitly excluded is in cfset. *)
    not (StringSet.subset fset cfset)

let is_subset v1 v2 = match v1, v2 with
  | Finite set1, Finite set2 -> StringSet.subset set1 set2
  | CoFinite set1, CoFinite set2 -> StringSet.subset set2 set1
  | Finite fset, CoFinite cfset ->
    (* The finite set must be in the complement *)
    StringSet.is_empty (StringSet.inter fset cfset)
  | CoFinite _, Finite _ -> false

let is_equal v1 v2 = match v1, v2 with
  | Finite set1, Finite set2
  | CoFinite set1, CoFinite set2 ->
    StringSet.equal set1 set2
  | _ -> false

let pretty_helper v =
  let open FormatExt in
  match v with
    | Finite set -> 
      if StringSet.cardinal set = 1 then
  text (StringSet.choose set)
      else
  StringSetExt.p_set text set
    | CoFinite set ->
      if StringSet.is_empty set then
        text "/.*/"
      else
        horz [ squish [ text "-"; StringSetExt.p_set text set; text "-" ] ]


let pretty v =
  FormatExt.to_string pretty_helper v

let example v = match v with
  | Finite set -> 
    if StringSet.is_empty set then
      None
    else
      Some (StringSet.choose set)
  | CoFinite set -> 
    let lengths = List.sort (compare) (List.map String.length (StringSetExt.to_list set)) in
    let min_gap = 
      let rec helper ls = match ls with
        | l1::l2::ls -> if (l1 + 1 < l2) then l1 + 1 else helper (l2::ls)
        | [l] -> l+1
        | [] -> 1 in
      helper lengths in
    Some (String.make min_gap 'X')
  

let rec set_to_nfa set = 
  let module R = PatReg in
  match (StringSet.fold
           (fun str nfa -> match nfa with 
           | None -> Some (R.singleton str)
           | Some nfa -> Some (R.union (R.singleton str) nfa))
           set
           None) with
  | None -> R.empty
  | Some nfa -> nfa

let to_nfa v = match v with
  | Finite set -> set_to_nfa set
  | CoFinite set -> PatReg.negate (set_to_nfa set)

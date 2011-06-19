type id = Id.t

(** language specified on pg. 599, excluding $\in$ *)
type set =
  | EmptySet
  | Var of id
  | Union of set * set
  | Diff of set * set

type term =
  | Imp of term * term
  | And of term * term
  | Not of term
  | Eql of set * set

let mk_inter (e : set) (f : set) : set = Diff (e, Diff (e, f))

let mk_is_subset (e : set) (f : set) : term =
  And (Eql (Diff (e, f), EmptySet), Not (Eql (e, f)))

(** forms specified in  Step A of pg. 601 *)
type lit =
  | IsUnion of id * id * id (* x = y union z *)
  | IsDiff of id * id * id
  | IsNotEqual of id * id
  | IsEqual of id * id

let is_IsNotEqual lit = match lit with
  | IsNotEqual _ -> true
  | _ -> false



module ToSat = struct

  open Sat
  let sat_of_lit lit = match lit with
    | IsUnion (x, y, z) -> Eql (Var x, Or (Var y, Var z))
    | IsDiff (x, y, z) -> Eql (Var x, And (Var y, Not (Var z)))
    | IsNotEqual (x, y) -> Not (Eql (Var x, Var y))
    | IsEqual (x, y) -> Eql (Var x, Var y)

  let to_sat q0 =
    let q0_sat = 
      List.fold_left (fun t1 t2 -> Sat.And (t1, t2)) 
	True (List.map sat_of_lit q0) in
    fun d -> And (sat_of_lit d, q0_sat)

end

let is_satisfiable (q : lit list) : bool =
  let (d_list, q0) = List.partition is_IsNotEqual q in
  let to_sat' = ToSat.to_sat q0 in
  List.for_all (fun d -> Sat.is_sat (to_sat' d)) d_list

open Prelude
open Sig

module Make (PAT : PAT) (SAT : SAT) = struct


  module SAT = SAT
  module PATV = SAT.EQ
  module PAT = PAT



  (* Step 1 of Lemma 1's proof *)
  let elim_empty_set (q : SAT.t list) = 
    let x0 = PATV.Var (Id.gen_id ()) in
    let rec subst_in_set (v : PATV.t) = match v with
      | PATV.Var _ -> v
      | PATV.Pat p -> if PAT.is_empty then x0 else v
      | PATV.Inter (x, y) -> PATV.Inter (subst_in_set x, subst_in_set y)
      | PATV.Diff (x, y) -> PATV.Diff (subst_in_set x, subst_in_set y)
      | PATV.Union (x, y) -> PATV.Union (subst_in_set x, subst_in_set y) in
    let rec subst_in_lit (l : SAT.t) = match l with
      | SAT.Not t -> SAT.Not (subst_in_lit t)
      | SAT.Var _ -> l
      | SAT.Eq (v1, v2) -> SAT.Eq (subst_in_set v1, subst_in_set v2)
      | _ -> failwith "expected literal" in
    let x0_is_empty = Eq (x0, PATV.Diff (x0, x0)) in
    x0_is_empty :: (map subst_in_lit q)

  let rec explode_set (v :  SATV.t)


  (* Step 2 of Lemma 1's proof *)
  let rec flatten_lit (q : SAT.t) : SAT.t list =
    let open SAT in
    let open PATV in
    match q with
      (* forms on pg. 602 *)
      | Eq (Var x, Union (Var y, Var z))
      | Eq (Var x, Diff (Var y, Var z))
      | Eq (Var x, Var y)
      | Not (Eq (Var x, Var y)) -> [q]


    let f (lit : SAT.t)

  (* [q] represents a conjunction of literals *)
  let mls_lits (q : SAT.t list) : bool = 
    failwith "Foo"

  let is_sat (formula : SAT.t) : bool =
    let conjunct_list = SAT.unconjunct (SAT.dnf formula) in
    List.exists (fun disjunct -> mls_lits (SAT.undisjunct disjunct))
      conjunct_list

(*


  (** forms specified in  Step A of pg. 601 *)
  type lit =
    | IsUnion of id * id * id (* x = y union z *)
    | IsDiff of id * id * id
    | IsNotEqual of id * id
    | IsEqual of id * id



let mk_inter (e : set) (f : set) : set = Diff (e, Diff (e, f))

let mk_is_subset (e : set) (f : set) : term =
  And (Eql (Diff (e, f), EmptySet), Not (Eql (e, f)))


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
*)

end

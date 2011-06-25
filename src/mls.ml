open Prelude
open Sig

module Make (PAT : PAT) (SAT : SAT) = struct


  module SAT = SAT
  module PATV = SAT.EQ
  module PAT = PAT


(* implement recursive function for Step 2 of Lemma 1
  - Perhaps this function will reduce operations on constant patterns. It would need to know of the empty set.

Recall that we still need to eliminate intersections, etc. as before on page 2 of the paper.
*)



  (* Step 1 of Lemma 1's proof *)
  let elim_empty_set (q : SAT.t list) : Id.t * SAT.t list = 
    let open SAT in
    let open PATV in
    let x0 = PATV.Var (Id.gen_id ()) in
    let rec subst_in_set (v : PATV.t) = match v with
      | Var _ -> v
      | Pat p -> if is_empty then x0 else v
      | Inter (x, y) -> Inter (subst_in_set x, subst_in_set y)
      | Diff (x, y) -> Diff (subst_in_set x, subst_in_set y)
      | Union (x, y) -> Union (subst_in_set x, subst_in_set y) in
    let rec subst_in_lit (l : SAT.t) = match l with
      | Not t -> Not (subst_in_lit t)
      | Var _ -> l
      | Eq (v1, v2) -> Eq (subst_in_set v1, subst_in_set v2)
      | _ -> failwith "expected literal" in
    let x0_is_empty = Eq (x0, Diff (x0, x0)) in
    (x0, x0_is_empty :: (map subst_in_lit q))

  let explode (t : EQ.t) : EQ.t * SAT.t list =
    let open SAT in
    let open PATV in
    let lits = ref [] in  
    let rec f t = match t with
      | Var _
      | Pat _ -> 
	t
      | Union (Var x, Var z) 
      | Diff (Var x, Var z) ->
	let x = Id.get_id () in lits := Eq (Var x, t); Var x
      | Union (t1, t2) ->
	let x = Id.get_it () in 
	lits := Eq (Var x, Union (f t1, f t2));
	Var x
      | Diff (t1, t2) ->
	let x = Id.get_it () in 
	lits := Eq (Var x, Diff (f t1, f t2));
	Var x
      | _ -> failwith "unexpected pattern" in
    let t' = f t in
    (t', !lits)

  (* Step 2 of Lemma 1's proof.
   
     [q] is a non-constant literal, either [Eq (x, y)] or [Not (Eq (x, y))]
  *)
  let flatten_lit (q : SAT.t) : SAT.t list =
    let open SAT in
    let open PATV in
    match q with
      (* forms on pg. 602; these are okay *)
      | Eq (Var x, Union (Var y, Var z))
      | Eq (Union (Var y, Var z), Var x) (* symmetric *)
      | Eq (Var x, Diff (Var y, Var z))
      | Eq (Diff (Var y, Var z), Var x) (* symmetric *)
      | Eq (Var x, Var y)
      | Not (Eq (Var x, Var y)) -> [q]
      (* everything else *)
      | Eq (x, y) ->
	let (x', lits1) = explode x in
	let (y', lits2) = explode y in
	lits1 @ lits2 :: (Eq (x, y))
      | Not (Eq (x, y)) ->
	let (x', lits1) = explode x in
	let (y', lits2) = explode y in
	lits1 @ lits2 :: (Not (Eq (x, y)))
      | _ -> failwith "unexpected pattern"

  (* TODO: transform to pure SAT *)
  let to_sat (e : SAT.t) : SimplSAT.t = match e with
    | Eq (Var x, Union (Var y, Var z)) -> 
      | Eq (Union (Var y, Var z), Var x) (* symmetric *)
      | Eq (Var x, Diff (Var y, Var z)) ->
      | Eq (Diff (Var y, Var z), Var x) (* symmetric *)
      | Eq (Var x, Var y)
      | Not (Eq (Var x, Var y)) -> [q]

  let find_model q0 =
    let q0_sat = to_sat q0 in
    fun d ->
      let d_sat = to_sat d
	SAT.is_sat SAT.And (d_sat, q0_sat)

  let is_Eq t = match t with
    | Eq _ -> true 
    | _ -> false in

  let subst (x : Id.t) (v : PATV.t) (t : SAT.t) : SAT.t =
    let open PATV in
    let open SAT in
    let rec f (t : PATV.t) = match t with
      | Var y -> if Id.compare x y then v else t
      | Pat _ -> t
      | Union (t1, t2) -> Union (f t1, f t2)
      | Diff (t1, t2) -> Diff (f t1, f t2)
      | Inter (t1, t2) -> Inter (f t1, f t2) in
    match t with
      | Eq (t1, t2) -> Eq (f t1, f t2)
      | Not (Eq (t1, t2)) -> Not (Eq (f t1, f t2))

  let is_eq_var t = match t with
    | Eq (Var _, Var _) 
    | Eq (Pat _, Pat _)
    | Eq (Var _, Pat _) -> true
    | _ -> false

  (* All terms [t] where [eq_var t] holds must be prefixed *)
  let rec elim_equiv_ordered (q : SAT.t list) = match q with
    | Eq (v, Var x) :: rest
    | Eq (Var x, v) :: rest -> elim_equiv_ordered (subst x v rest)
    | _ -> q

  let elim_equiv (q : SAT.t list) = 
    let (eqs, not_eqs) = List.partition is_eq_var q in
    elim_equiv_ordered (eqs @ not_eqs)

  (* determines if the conjunct, [q], is satisfiable *)
  let mls_lits (q : SAT.t list) : bool = 
    (* Eliminate the empty set. *)
    let (empty_set, q) = elim_empty_set q in
    (* Transform all literals to [x <> y], [x = y], [x = y U z], and
       [x = y \ z] where [x], [y], and [z] denote variables or set-literals. *)
    let q = L.concat (map flatten_lit q) in
    (* eliminate [x=y] equalities by substitution *)
    let q = elim_equiv q in

    (* TODO: need to visualize formulae at this point. Fix up bugs and
       get it to unit testing stage. *)

    (* q0 is a conjunction of equalities; d is a conjunction on inequalities
       (Step A) *)
    let (q0, d) = List.partition is_Eq q in
    (* if a model exists then it is satisfiable, [true] *)
    List.exists (find_model q0) d


  (* [formula] is an arbitrary propositional formula with embedded set
     constraints. *)
  let is_sat (formula : SAT.t) : bool =
    (* TODO: eliminate intersection of sets *)
    List.exists
      (fun conjunct -> mls_lits (SAT.unconjunct conjunct))
      (SAT.undisjunct (SAT.dnf formula))

(*

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

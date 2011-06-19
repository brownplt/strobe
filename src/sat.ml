open Prelude
open Sig
(* A SAT solver based on the code in the article:

   S. Conchon, J. Kanig, and S. Lescuyer. SAT-MICRO: petit mais costaud! In
   Journees Francophones des Langages Applicatifs. January 2008.

   The translation to CNF is based on a solution, written in Haskell,
   to a homework assignment that Arjun did in college. The solution includes a 
   proof of correctness.
*)

module Make (EQ : EQ) : (SAT with module EQ = EQ) = struct

  module EQ = EQ

  type t =
    | And of t * t
    | Or of t * t
    | Not of t
    | Var of Id.t
    | True
    | False
    | Imp of t * t
    | Eq of EQ.t * EQ.t

  let is_atom t = match t with
    | True 
    | False
    | Var _ 
    | Not (Var _) -> true
    | _ -> false
    
  let rec is_disjunct t = match t with
    | Or (x, y) -> is_disjunct x && is_disjunct y
    | _ -> is_atom t

  let rec is_conjunct t = match t with
    | And (x, y) -> is_conjunct x && is_conjunct y
    | _ -> is_atom t
      
  let rec cnf (t : t) = match t with
    | True 
    | False
    | Var _ 
    | Eq _ 
    | Not (Eq _)
    | Not (Var _) -> t
    | Not True -> False
    | Not False -> True
    | Not (Not x) -> cnf x
    | Imp (x, y) -> cnf (Or (Not x, y))
    (* not (x => y) = not ((not x) or y)
                    = (not (not x)) and (not y)
                    = x and (not y) *)
    | Not (Imp (x, y)) -> cnf (And (x, Not y))
    | Not (Or (x, y)) -> cnf (And (Not x, Not y))
    | Not (And (x, y)) -> cnf (Or (Not x, Not y))
    | And (x, y) -> And (cnf x, cnf y)
    | Or (And (x, y), z) -> And (cnf (Or (x, z)), cnf (Or (y, z)))
    | Or (x, And (y, z)) -> And (cnf (Or (x, y)), cnf (Or (x, z)))
    | Or (x, y) -> 
      let x' = cnf x in
      let y' = cnf y in
      if is_disjunct x' && is_disjunct y' then
	Or (x', y')
      else
	cnf (Or (x', y'))

  let rec dnf (t : t) = match t with
    | True 
    | False
    | Var _ 
    | Eq _ 
    | Not (Eq _)
    | Not (Var _) -> t
    | Not True -> False
    | Not False -> True
    | Not (Not x) -> dnf x
    | Imp (x, y) -> dnf (Or (Not x, y))
    (* not (x => y) = not ((not x) or y)
                    = (not (not x)) and (not y)
                    = x and (not y) *)
    | Not (Imp (x, y)) -> dnf (And (x, Not y))
    | Not (Or (x, y)) -> dnf (And (Not x, Not y))
    | Not (And (x, y)) -> dnf (Or (Not x, Not y))
    | Or (x, y) -> Or (dnf x, dnf y)
    | And (Or (x, y), z) -> Or (dnf (And (x, z)), dnf (And (y, z)))
    | And (x, Or (y, z)) -> Or (dnf (And (x, y)), dnf (And (x, z)))
    | And (x, y) -> 
      let x' = dnf x in
      let y' = dnf y in
      if is_conjunct x' && is_conjunct y' then
	And (x', y')
      else
	dnf (And (x', y'))

  let rec unconjunct (t : t) : t list = match t with
    | And (x, y) -> unconjunct x @ unconjunct y
    | _ -> [t]

  let rec undisjunct (t : t) : t list = match t with
    | Or (x, y) -> undisjunct x @ undisjunct y
    | _ -> [t]


  module L = struct

    type t = { v : Id.t; is_not: bool }

    let gen_var () =  { v = Id.gen_id (); is_not = false }
      
    let mk_not t = { t with is_not = not t.is_not }

    let compare t1 t2 = 
      let c = Id.compare t1.v t2.v in
      if c != 0 then c
      else Pervasives.compare t1.is_not t2.is_not

    let from_id x = { v = x; is_not = false }
      
    let mk_var x = { v = Id.id_of_string x; is_not = false }
  end

  module Dpll = struct
    exception Unsat
    exception Sat

    module S = Set.Make(L)

    type t = { gamma : S.t; delta : L.t list list }

    let rec assume env f =
      if S.mem f env.gamma then env
      else bcp { gamma = S.add f env.gamma; delta = env.delta }

    and bcp env =
      List.fold_left
	(fun env l -> 
	  try
	    let l = List.filter
	      (fun f -> 
		if S.mem f env.gamma then raise Exit;
		not (S.mem (L.mk_not f) env.gamma)) l in
	    match l with
	      | [] -> raise Unsat
	      | [f] -> assume env f
	      | _ -> { env with delta = l :: env.delta }
	  with Exit -> env)
	{ env with delta=[] }
	env.delta

    let rec unsat env =
      try match env.delta with
	  [] -> raise Sat
	| ([_] | []) :: _ -> assert false
	| (a :: _) :: _ ->
	  let _ = try unsat (assume env a) with Unsat -> () in
	  unsat (assume env (L.mk_not a))
      with Unsat -> ()
	
    let unsyntax_disjunct term =
      let rec f t = match t with
      | Or (x, y) -> f x @ f y
      | True -> raise Exit (* trivially true *)
      | False -> []
      | Eq (a, b) -> if EQ.is_equal a b then raise Exit else []
      | Not (Var x) -> [L.mk_not (L.from_id x)]
      | Var x -> [L.from_id x]
      | _ -> raise (Invalid_argument "expected disjunction of atoms") in
      try f term 
      with Exit -> [ L.gen_var () ]

    let unsyntax_conjunct term = 
      let rec f t = match t with
	| And (x, y) -> f x @ f y
	| _ -> [ unsyntax_disjunct t ] in
      f term
	
    let dpll e =
      let cnf_repr = unsyntax_conjunct (cnf e) in
      try 
	let _ = unsat (bcp { gamma = S.empty; delta = cnf_repr }) in
	false
      with 
	| Sat -> true 
	| Unsat -> false

  end
	      

  let is_sat = Dpll.dpll

end

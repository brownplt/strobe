(* A SAT solver based on the code in the article:

   S. Conchon, J. Kanig, and S. Lescuyer. SAT-MICRO: petit mais costaud! In
   Journees Francophones des Langages Applicatifs. January 2008.

   The translation to CNF is based on a solution, written in Haskell,
   to a homework assignment that Arjun did in college. The solution includes a 
   proof of correctness.
*)
type t =
  | And of t * t
  | Or of t * t
  | Not of t
  | Var of Id.t
  | True
  | False
  | Imp of t * t
  | Eql of t * t

let is_atom t = match t with
  | True 
  | False
  | Var _ 
  | Not (Var _) -> true
  | _ -> false
    
let rec is_disjunct t = match t with
  | Or (x, y) -> is_disjunct x && is_disjunct y
  | _ -> is_atom t

let rec to_cnf (t : t) = match t with
  | True 
  | False
  | Var _ 
  | Not (Var _) -> t
  | Not True -> False
  | Not False -> True
  | Not (Not x) -> to_cnf x
  | Imp (x, y) -> to_cnf (Or (Not x, y))
  | Eql (x, y) -> to_cnf (Or (And (x, y), (And (Not x, Not y))))
  | Not (Eql (x, y)) -> to_cnf (Or (And (x, Not y), And (y, Not x)))
    (* not (x => y) = not ((not x) or y)
                    = (not (not x)) and (not y)
                    = x and (not y) *)
  | Not (Imp (x, y)) -> to_cnf (And (x, Not y))
  | Not (Or (x, y)) -> to_cnf (And (Not x, Not y))
  | Not (And (x, y)) -> to_cnf (Or (Not x, Not y))
  | And (x, y) -> And (to_cnf x, to_cnf y)
  | Or (And (x, y), z) -> And (to_cnf (Or (x, z)), to_cnf (Or (y, z)))
  | Or (x, And (y, z)) -> And (to_cnf (Or (x, y)), to_cnf (Or (x, z)))
  | Or (x, y) -> 
    let x' = to_cnf x in
    let y' = to_cnf y in
    if is_disjunct x' && is_disjunct y' then
      Or (x', y')
    else
      to_cnf (Or (x', y'))

module type LIT = sig

  type t
  val mk_not : t -> t
  val mk_var : string -> t
  val from_id : Id.t -> t
  val gen_var : unit -> t
  val compare : t -> t -> int

end

module DPLL (L : LIT) : sig
  val dpll : t -> bool
end  = struct

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

  let dpll cnf =
    let cnf_repr = unsyntax_conjunct (to_cnf cnf) in
    try 
      let _ = unsat (bcp { gamma = S.empty; delta = cnf_repr }) in
      false
    with 
      | Sat -> true 
      | Unsat -> false



end
	      
module Lit : LIT = struct


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

module Dpll = DPLL (Lit)

let is_sat = Dpll.dpll

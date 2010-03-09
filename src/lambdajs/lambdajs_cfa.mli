open Prelude
open Lambdajs_cps
open Lambdajs_lattice

(** This is a 1-CFA *)
type context = int

val envs : (context, env) Hashtbl.t
val heaps : (context, heap) Hashtbl.t



(** [reachable] maps all nodes that are statically reachable.

    We can debug the analysis by ensuring that all the variable-sets are 
    non-empty. *)    
val reachable : (int, cpsexp) Hashtbl.t

(** [call_graph] maps nodes for application sites to sets of function names. *)
val call_graph : (int, IntSet.t) Hashtbl.t

val blocks : (int, cpsexp) Hashtbl.t



val cfa : cpsexp -> unit



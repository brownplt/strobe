open Prelude
open Lambdajs_cps
open Lambdajs_lattice


(** [reachable] maps all nodes that are statically reachable.

    We can debug the analysis by ensuring that all the variable-sets are 
    non-empty. *)    
val reachable : (int, cpsexp) Hashtbl.t


val envs : (int, AV.env) Hashtbl.t

val cfa : cpsexp -> unit

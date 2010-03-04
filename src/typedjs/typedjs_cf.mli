open Prelude
open Typedjs_lattice
open Typedjs_cps

val typed_cfa : env -> cpsexp -> unit

val bound_id_map : (pos * id, node) Hashtbl.t
val envs : (node, env) Hashtbl.t
val heaps : (node, heap) Hashtbl.t


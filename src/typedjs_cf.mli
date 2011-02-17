open Prelude
open Typedjs_lattice
open Typedjs_cps

val set_op_env : Typedjs_env.Env.env -> unit

val typed_cfa : Typedjs_syntax.typ IdMap.t -> env -> cpsexp -> unit

val bound_id_map : (pos * id, node) Hashtbl.t
val envs : (node, env) Hashtbl.t
val heaps : (node, heap) Hashtbl.t


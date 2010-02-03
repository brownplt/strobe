open Prelude

open Typedjs_syntax

type env

val any_runtime_typ : RTSet.t

val empty_env : env

val union_abs_value : abs_value -> abs_value -> abs_value

val union_env : env -> env -> env

val lookup_env : id -> env -> abs_value


(** [bind_env x v env] performs a conventional binding and also updates
    bindings that reference [x]. *)
val bind_env : id -> abs_value -> env -> env

val pretty_env : Format.formatter -> env -> unit

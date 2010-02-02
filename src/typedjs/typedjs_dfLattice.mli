open Prelude

type runtime_type =
    RTNumber
  | RTString
  | RTBoolean
  | RTFunction
  | RTObject
  | RTUndefined

module RTSet : Set.S
  with type elt = runtime_type

module RTSetExt : SetExt.S
  with type elt = runtime_type
  and type t = RTSet.t

type abs_value =
    AVType of RTSet.t
  | AVTypeof of id
  | AVString of string
  | AVTypeIs of id * RTSet.t

type env

val empty_env : env

val union_abs_value : abs_value -> abs_value -> abs_value

val union_env : env -> env -> env

val lookup_env : id -> env -> abs_value


(** [bind_env x v env] performs a conventional binding and also updates
    bindings that reference [x]. *)
val bind_env : id -> abs_value -> env -> env

val pretty_abs_value : Format.formatter -> abs_value -> unit

val pretty_env : Format.formatter -> env -> unit

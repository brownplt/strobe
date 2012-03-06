open Prelude
open Typedjs_syntax

exception Kind_error of string

(** [new_prim_typ s] adds [s] as a new primitive type.

    It will throw a [Kind_error] exception if [s] is already defined.
    After [new_prim_typ s] succeeds, [kind_check kind_env (TPrim s) = KStar]. *)
val new_prim_typ : string -> unit

val kind_check : kind IdMap.t -> typ -> kind

val list_prims : unit -> string list

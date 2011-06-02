open Prelude
open Typedjs_syntax

exception Kind_error of string

(** Decides if two types are syntactically equal. This helps subtyping. *)
val simpl_equiv : typ -> typ -> bool

val kind_check : kind IdMap.t -> kind IdMap.t -> typ -> kind

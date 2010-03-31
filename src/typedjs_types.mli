(** Definitions of builtin types, subtyping, etc. *)
open Prelude
open Typedjs_syntax

(**[0] Builtin types *)

val typ_str : typ

val typ_regexp : typ

val typ_num : typ

val typ_int : typ

val typ_bool : typ

val typ_null : typ

val typ_undef : typ

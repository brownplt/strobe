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

(**[0] Subtyping *)

val subtype : typ -> typ -> bool

(** [subtypes typs1 typs2] applies [subtype] pairwise to the elements of [typs1]
    and [typs2]. If the lists have unequal lengths, it returns [false]. *)
val subtypes : typ list -> typ list -> bool

val typ_union : typ -> typ -> typ

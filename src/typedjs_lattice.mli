open Prelude
open Typedjs_cps
open Typedjs_syntax

type loc = 
  | Loc of int
  | LocField of int * string

module Loc : sig
  type t = loc
  val compare : t -> t -> int
  val pp : t -> FormatExt.printer
end

type av =
  | ASet of RTSet.t
  | ADeref of Loc.t
  | ARef of Loc.t
  | ALocTypeof of Loc.t
  | ALocTypeIs of Loc.t * RTSet.t
  | AString of string
  | AClosure of int * id list * cpsexp

type env

type heap

val singleton : RT.t -> av

val rtany : RTSet.t

val any : av

val runtime : Typedjs_syntax.typ -> av

val empty : av

val p_av : av -> FormatExt.printer

val av_union : heap -> av -> av -> av

val union_env : heap -> env -> env -> env

val lookup : id -> env -> av

val bind : id -> av -> env -> env

val p_env : env -> FormatExt.printer

val p_heap : heap -> FormatExt.printer

val empty_env : env

val deref : Loc.t -> heap -> RTSet.t

val set_ref : Loc.t -> RTSet.t -> heap -> heap

val to_set : heap -> av -> RTSet.t

val union_heap : heap -> heap -> heap

val empty_heap : heap

val compare_heap : heap -> heap -> int

val compare_env : env -> env -> int

val escape_env : heap -> env -> env

val escape_heap : heap -> heap

val rt_of_typ : typ -> RTSet.t

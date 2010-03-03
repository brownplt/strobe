open Prelude
open Typedjs_cps

type loc = 
  | Loc of int
  | LocField of int * string

module Loc : sig
  type t = loc
  val compare : t -> t -> int
end

module RT : sig
  type t =
    | Number
    | String
    | Boolean
    | Function
    | Object
    | Undefined

  val compare : t -> t -> int
end

module RTSet : Set.S with type elt = RT.t
module RTSetExt : SetExt.S with type elt = RT.t with type t = RTSet.t

module Heap : Map.S
  with type key = Loc.t

module HeapExt : MapExt.S 
  with type key = Loc.t
  with type +'a t = 'a Heap.t

type av =
  | ASet of RTSet.t
  | ATypeof of id
  | ATypeIs of id * RTSet.t
  | AString of string
  | AClosure of int * id list * cpsexp

type env

type heap = av Heap.t

val singleton : RT.t -> av

val any : av

val runtime : Typedjs_syntax.typ -> av

val empty : av

val p_av : av -> FormatExt.printer

val av_union : av -> av -> av

val union_env : env -> env -> env

val lookup : id -> env -> av

val bind : id -> av -> env -> env

val p_env : env -> FormatExt.printer

val empty_env : env

val deref : Loc.t -> heap -> av

val set_ref : Loc.t -> av -> heap -> heap

val to_set : av -> RTSet.t

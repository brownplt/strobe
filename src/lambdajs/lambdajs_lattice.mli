open Prelude
open Lambdajs_cps

type loc = 
  | Loc of int
  | LocField of int * string

module Loc : sig
  type t = loc
  val compare : t -> t -> int
end

module AV : sig
  type t = 
    | ANumber
    | ABool
    | AString
    | AConst of Exprjs_syntax.const
    | ARef of loc
    | AObj of loc IdMap.t
    | AClosure of int * id list * cpsexp
  
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
  
module AVSet : Set.S with type elt = AV.t

module AVSetExt : SetExt.S 
  with type elt = AV.t
  and type t = AVSet.t

type av =
  | ASet of AVSet.t
  | ALocTypeof of Loc.t
  | ALocTypeIs of Loc.t * RTSet.t
  | ADeref of Loc.t


type env

type heap

val singleton : AV.t -> av

val empty : av

val p_av : av -> FormatExt.printer

val to_set : heap -> av -> AVSet.t

val av_union : heap -> av -> av -> av

val union_env : heap -> env -> env -> env

val lookup : id -> env -> av

val bind : id -> av -> env -> env

val p_env : env -> FormatExt.printer

val empty_env : env

val deref : Loc.t -> heap -> AVSet.t

val set_ref : Loc.t -> AVSet.t -> heap -> heap

val union_heap : heap -> heap -> heap

val empty_heap : heap

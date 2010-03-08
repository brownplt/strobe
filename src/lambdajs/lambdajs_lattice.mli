open Prelude
open Lambdajs_cps
open FormatExt

type loc = 
  | Loc of int
  | LocField of int * string

module Loc : sig
  type t = loc
  val compare : t -> t -> int
  val pp : t -> FormatExt.printer
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

  val pp : t -> printer
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


module Range : sig
  type bound =
    | Int of int
    | PosInf
    | NegInf

  type t = 
    | Set of AVSet.t
    | Range of bound * bound

  val compare : t -> t -> int
  
  val pp : t -> printer

  val up : t -> AVSet.t
end
  

type av =
  | ARange of Range.t
  | ALocTypeof of Loc.t
  | ALocTypeIs of Loc.t * RTSet.t
  | ADeref of Loc.t


type env

type heap

val singleton : AV.t -> av

val empty : av

val p_av : av -> FormatExt.printer

val to_set : heap -> av -> AVSet.t

val to_range : heap -> av -> Range.t

val av_union : heap -> av -> av -> av

val union_env : heap -> env -> env -> env

val lookup : id -> env -> av

val bind : id -> av -> env -> env

val p_env : env -> FormatExt.printer

val p_heap : heap -> FormatExt.printer

val empty_env : env

val deref : Loc.t -> heap -> Range.t

val set_ref : Loc.t -> Range.t -> heap -> heap

val union_heap : heap -> heap -> heap

val empty_heap : heap

val compare_av : av -> av -> int

val compare_heap : heap -> heap -> int

val compare_env : env -> env -> int

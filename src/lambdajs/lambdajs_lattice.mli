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
  
module AVSet : Set.S with type elt = AV.t

module AVSetExt : SetExt.S 
  with type elt = AV.t
  and type t = AVSet.t

module Heap : Map.S
  with type key = Loc.t

module HeapExt : MapExt.S 
  with type key = Loc.t
  with type +'a t = 'a Heap.t

type av =
  | ASet of AVSet.t

type env = av IdMap.t

type heap = av Heap.t

val singleton : AV.t -> av

val empty : av

val p_av : av -> FormatExt.printer

val av_union : av -> av -> av

val union_env : env -> env -> env


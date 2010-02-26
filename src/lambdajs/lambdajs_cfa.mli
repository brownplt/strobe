open Prelude
open Lambdajs_cps

module rec AV 
  : sig
    type t = 
      | ANumber
      | ABool
      | AString
      | AConst of Exprjs_syntax.const
      | ARef of int
      | AObj of (string * avs) list
      | AArr of avs list
      | AClosure of int * id list * cpsexp

    and avs = AVSet.t
    and env = avs IdMap.t
    val compare : t -> t -> int
    val compare_env : env -> env -> int
  end
  
and AVSet 
  : Set.S with type elt = AV.t 

module AVSetExt : SetExt.S 
  with type elt = AV.t
  and type t = AVSet.t


val envs : (int, AV.env) Hashtbl.t

val cfa : cpsexp -> unit

val p_av :AV.t -> FormatExt.printer

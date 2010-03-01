open Prelude
open Lambdajs_cps

    
module rec AV : sig

  type t = 
    | ANumber
    | ABool
    | AString
    | AConst of Exprjs_syntax.const
    | ARef of int
    | AObj of avs IdMap.t
    | AArr of avs list
    | AClosure of int * id list * cpsexp
  and avs = AVSet.t
  and env = avs IdMap.t
  
  val compare : t -> t -> int

  val compare_env : env -> env -> int 
end 
= struct
  type t = 
    | ANumber
    | ABool
    | AString
    | AConst of Exprjs_syntax.const
    | ARef of int
    | AObj of avs IdMap.t
    | AArr of avs list
    | AClosure of int * id list * cpsexp
  and avs = AVSet.t
  and env = avs IdMap.t
      
  let compare = Pervasives.compare 

  let compare_env env1 env2 = 
    IdMap.compare AVSet.compare env1 env2
end
  
and AVSet 
  : Set.S with type elt = AV.t 
  = Set.Make (AV)

module AVSetExt = SetExt.Make (AVSet)

open AV

open FormatExt

let rec  p_av av  = match av with
  | AConst c -> Exprjs_pretty.p_const c
  | ARef x -> int x
  | AObj dict ->
      IdMapExt.p_map text (AVSetExt.p_set p_av) dict
  | AArr _ -> text "array"
  | AClosure (n, args, _) -> 
      text ("closure" ^ string_of_int n)
      

let union_env (env1 : AV.env) (env2 : AV.env) : AV.env = 
  IdMapExt.join AVSet.union  env1 env2

open Prelude
open Lambdajs_cps

type loc = 
  | Loc of int
  | LocField of int * string
    
module AV = struct
  type t = 
    | ANumber
    | ABool
    | AString
    | AConst of Exprjs_syntax.const
    | ARef of loc
    | AObj of loc IdMap.t
    | AClosure of int * id list * cpsexp
      
  let compare = Pervasives.compare 
end

module Loc = struct

  type t = loc

  let compare = Pervasives.compare

end

  
module AVSet = Set.Make (AV)

module AVSetExt = SetExt.Make (AVSet)

type env = AVSet.t IdMap.t

module Heap = Map.Make (Loc)
module HeapExt = MapExt.Make (Loc) (Heap)

open AV

open FormatExt

let p_loc loc = match loc with
  | Loc n -> int n
  | LocField (n, f) -> sep [ int n; text f ]

let rec p_av av  = match av with
  | AConst c -> Exprjs_pretty.p_const c
  | ARef l -> p_loc l
  | AObj dict ->
      IdMapExt.p_map text p_loc dict
  | AClosure (n, args, _) -> 
      text ("closure" ^ string_of_int n)
      

let union_env (env1 : env) (env2 : env) : env = 
  IdMapExt.join AVSet.union  env1 env2

type heap = AVSet.t Heap.t

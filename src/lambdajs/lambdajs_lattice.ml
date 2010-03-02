open Prelude
open Lambdajs_cps

type loc = 
  | Loc of int
  | LocField of int * string

module Loc = struct

  type t = loc

  let compare = Pervasives.compare

  open FormatExt

  let pp loc = match loc with
    | Loc n -> int n
    | LocField (n, f) -> sep [ int n; text f ]


end


    
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

  open FormatExt

  let pp v = match v with
  | AConst c -> Exprjs_pretty.p_const c
  | ARef l -> Loc.pp l
  | AObj dict -> IdMapExt.p_map text Loc.pp dict
  | AClosure (n, args, _) -> text ("closure" ^ string_of_int n)


end

  
module AVSet = Set.Make (AV)
module AVSetExt = SetExt.Make (AVSet)
module Heap = Map.Make (Loc)
module HeapExt = MapExt.Make (Loc) (Heap)



type av =
  | ASet of AVSet.t

type env = av IdMap.t

type heap = av Heap.t

open AV

open FormatExt


let rec p_av av = match av with
  | ASet s -> AVSetExt.p_set AV.pp s

let singleton t = ASet (AVSet.singleton t)

let empty = ASet AVSet.empty

let av_union av1 av2 = match av1, av2 with
  | ASet s1, ASet s2 -> ASet (AVSet.union s1 s2)

let union_env (env1 : env) (env2 : env) : env = 
  IdMapExt.join av_union  env1 env2



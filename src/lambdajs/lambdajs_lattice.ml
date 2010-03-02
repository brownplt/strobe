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

module RT = struct
  type t =
    | Number
    | String
    | Boolean
    | Function
    | Object
    | Undefined

  let compare = Pervasives.compare

  open FormatExt

  let pp v = match v with
    | Number -> text "number"
    | String -> text "string"
    | Boolean -> text "boolean"
    | Function -> text "function"
    | Object -> text "object"
    | Undefined -> text "undefined"

end

module AVSet = Set.Make (AV)
module AVSetExt = SetExt.Make (AVSet)
module RTSet = Set.Make (RT)
module RTSetExt = SetExt.Make (RTSet)
module Heap = Map.Make (Loc)
module HeapExt = MapExt.Make (Loc) (Heap)



type av =
  | ASet of AVSet.t
  | ATypeof of id
  | ATypeIs of id * RTSet.t

type env = av IdMap.t

type heap = av Heap.t

open AV

open FormatExt


let rec p_av av = match av with
  | ASet s -> AVSetExt.p_set AV.pp s
  | ATypeof x -> sep [ text "typeof"; text x ]
  | ATypeIs (x, t) -> sep [ text "typeis";  RTSetExt.p_set RT.pp t ]

let singleton t = ASet (AVSet.singleton t)

let empty = ASet AVSet.empty

let rec av_union av1 av2 = match av1, av2 with
  | ASet s1, ASet s2 -> ASet (AVSet.union s1 s2)
  | ATypeof x, ATypeof y when x = y -> ATypeof x
  | ATypeIs (x, s), ATypeIs (y, t) when x = y -> ATypeIs (x, RTSet.union s t)
  | ATypeof _, _ -> av_union (singleton AString) av2
  | ATypeIs _, _ -> av_union (singleton ABool) av2
  | _, ATypeof _ -> av_union av1 (singleton AString)
  | _, ATypeIs _ -> av_union av1 (singleton ABool)

let union_env (env1 : env) (env2 : env) : env = 
  IdMapExt.join av_union  env1 env2

let p_env env = IdMapExt.p_map text p_av env


let lookup (x : id) (env : env) : av=
  try
    IdMap.find x env
  with Not_found ->
    eprintf "%s is unbound in the abstract environment " (to_string p_env env);
    raise Not_found

let bind (x : id) (v : av) (env : env) : env = IdMap.add x v env

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
  | ABool -> text "boolean"
  | ANumber -> text "number"
  | AString -> text "string"

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
  | ALocTypeof of Loc.t
  | ALocTypeIs of Loc.t * RTSet.t
  | ADeref of Loc.t

type env = av IdMap.t

type heap = AVSet.t Heap.t

open AV

open FormatExt


let rec p_av av = match av with
  | ASet s -> AVSetExt.p_set AV.pp s
  | ALocTypeof x -> sep [ text "typeof"; Loc.pp x ]
  | ALocTypeIs (x, t) -> sep [ text "typeis";  Loc.pp x; 
                               RTSetExt.p_set RT.pp t ]
  | ADeref l -> sep [ text "deref"; Loc.pp l ]

let singleton t = ASet (AVSet.singleton t)

let empty = ASet AVSet.empty

let deref loc heap =
  try 
    Heap.find loc heap
  with Not_found ->
    eprintf "%s is not a location in the heap " (to_string Loc.pp loc);
    raise Not_found


let rec to_set heap v = match v with
  | ASet set -> set
  | ALocTypeof _ -> AVSet.singleton AString
  | ALocTypeIs _ ->  AVSet.singleton AString
  | ADeref l -> deref l heap

let rec av_union heap av1 av2 = match av1, av2 with
  | ASet s1, ASet s2 -> 
      ASet (AVSet.union s1 s2)
  | ALocTypeof x, ALocTypeof y when x = y -> 
      ALocTypeof x
  | ALocTypeIs (x, s), ALocTypeIs (y, t) when x = y ->
      ALocTypeIs (x, RTSet.union s t)
  | ADeref x, ADeref y when x = y -> 
      ADeref x
  | _ -> ASet (AVSet.union (to_set heap av1) (to_set heap av2))

let union_env heap (env1 : env) (env2 : env) : env = 
  IdMapExt.join (av_union heap) env1 env2

let p_env env = IdMapExt.p_map text p_av env

let p_heap heap = HeapExt.p_map Loc.pp (AVSetExt.p_set AV.pp) heap

let lookup (x : id) (env : env) : av=
  try
    IdMap.find x env
  with Not_found ->
    eprintf "%s is unbound in the abstract environment" x;
    raise Not_found

let bind (x : id) (v : av) (env : env) : env = IdMap.add x v env

let empty_env = IdMap.empty

let union_heap h1 h2 = HeapExt.join AVSet.union h1 h2

let set_ref loc value heap =
  Heap.add loc value heap


let empty_heap = Heap.empty

let compare_av v1 v2 = match v1, v2 with
  | ASet s1, ASet s2 -> AVSet.compare s1 s2
  | ALocTypeIs (l1, s1), ALocTypeIs (l2, s2) ->
      if l1 = l2 then
        RTSet.compare s1 s2
      else compare l1 l2
  | _ -> compare v1 v2

let compare_heap h1 h2 = Heap.compare AVSet.compare h1 h2

let compare_env env1 env2 = IdMap.compare compare_av env1 env2

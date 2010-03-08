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
    | LocField (n, f) -> text (string_of_int n ^ ":" ^ f)


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
  | ARef l ->  sep [ text "*"; Loc.pp l ]
  | AObj dict ->
      IdMapExt.p_map (fun s -> text ("\"" ^ String.escaped s ^ "\"")) Loc.pp
        dict
  | AClosure (n, args, _) -> text ("closure" ^ string_of_int n)
  | ABool -> text "boolean"
  | ANumber -> text "number"
  | AString -> text "string"

end

module AVSet = Set.Make (AV)
module AVSetExt = SetExt.Make (AVSet)
module RTSet = Set.Make (RT)
module RTSetExt = SetExt.Make (RTSet)


module Range = struct

  type bound =
    | Int of int
    | PosInf
    | NegInf


  type t = 
    | Set of AVSet.t
    | Range of bound * bound


  let compare v1 v2 = match v1, v2 with
    | Set s1, Set s2 -> AVSet.compare s1 s2
    | _ -> Pervasives.compare v1 v2

  let compare_bound b1 b2 = match b1, b2 with
      | Int n1, Int n2 -> n1 - n2
      | PosInf, PosInf -> 0
      | NegInf, NegInf -> 0
      | NegInf, _ -> -1
      | PosInf, _ -> 1
      | _, NegInf -> 1
      | _, PosInf -> -1

  let min x y = if compare_bound x y < 0 then x else y

  let max x y = if compare_bound x y > 0 then x else y



  let up v = match v with
    | Set s -> s
    | Range _ -> AVSet.singleton AV.ANumber

  let union v1 v2 = match v1, v2 with
    | Set s1, Set s2 -> Set (AVSet.union s1 s2)
    | Range (lb1, ub1), Range (lb2, ub2) -> Range (min lb1 lb2, max ub1 ub2)
    | _ -> Set (AVSet.union (up v1) (up v2))

  open FormatExt

  let pp_bound v = match v with
    | Int n -> int n
    | PosInf -> text "+inf"
    | NegInf -> text "-inf"

  let pp v = match v with
    | Set s -> AVSetExt.p_set AV.pp s
    | Range (b1, b2) -> if compare_bound b1 b2 = 0 
        then parens [text "forall x . x =="; pp_bound b1 ]
        else parens [text "forall x . x >="; pp_bound b1; 
                     text "^ x <="; pp_bound b2 ]

end 


module Heap = Map.Make (Loc)
module HeapExt = MapExt.Make (Loc) (Heap)

type av =
  | ARange of Range.t
  | ALocTypeof of Loc.t
  | ALocTypeIs of Loc.t * RTSet.t
  | ADeref of Loc.t


type heap = Range.t Heap.t

type env = av IdMap.t


open AV

open FormatExt


let rec p_av av = match av with
  | ARange r -> Range.pp r
  | ALocTypeof x -> sep [ text "typeof"; Loc.pp x ]
  | ALocTypeIs (x, t) -> sep [ text "typeis";  Loc.pp x; 
                               RTSetExt.p_set RT.pp t ]
  | ADeref l -> sep [ text "deref"; Loc.pp l ]

let singleton t = ARange (Range.Set (AVSet.singleton t))

let empty = ARange (Range.Set AVSet.empty)

let deref loc heap =
  try 
    Heap.find loc heap
  with Not_found ->
    eprintf "%s is not a location in the heap " (to_string Loc.pp loc);
    raise Not_found


let rec to_set (heap : heap)  v = match v with
  | ARange r -> Range.up r
  | ALocTypeof _ -> AVSet.singleton AString
  | ALocTypeIs _ ->  AVSet.singleton AString
  | ADeref l -> Range.up (deref l heap)

let to_range (heap : heap) v = match v with
  | ARange r -> r
  | ALocTypeof _ -> Range.Set (AVSet.singleton AString)
  | ALocTypeIs _ ->  Range.Set (AVSet.singleton AString)
  | ADeref l -> deref l heap

let rec av_union heap av1 av2 = match av1, av2 with
  | ARange r1, ARange r2 -> ARange (Range.union r1 r2)
  | ALocTypeof x, ALocTypeof y when x = y -> 
      ALocTypeof x
  | ALocTypeIs (x, s), ALocTypeIs (y, t) when x = y ->
      ALocTypeIs (x, RTSet.union s t)
  | ADeref x, ADeref y when x = y -> 
      ADeref x
  | _ -> ARange (Range.Set (AVSet.union (to_set heap av1) (to_set heap av2)))

let union_env heap (env1 : env) (env2 : env) : env = 
  IdMapExt.join (av_union heap) env1 env2

let p_env env = IdMapExt.p_map text p_av env

let p_heap heap = HeapExt.p_map Loc.pp Range.pp heap

let lookup (x : id) (env : env) : av=
  try
    IdMap.find x env
  with Not_found ->
    eprintf "%s is unbound in the environment:\n%s\n" x
    (FormatExt.to_string p_env env);
    
    raise Not_found

let bind (x : id) (v : av) (env : env) : env = IdMap.add x v env

let empty_env = IdMap.empty

let union_heap h1 h2 = HeapExt.join Range.union h1 h2

let set_ref loc value heap =
  Heap.add loc value heap


let empty_heap = Heap.empty

let compare_av v1 v2 = match v1, v2 with
  | ARange r1, ARange r2 -> Range.compare r1 r2
  | ALocTypeIs (l1, s1), ALocTypeIs (l2, s2) ->
      if l1 = l2 then
        RTSet.compare s1 s2
      else compare l1 l2
  | _ -> compare v1 v2

let compare_heap h1 h2 = Heap.compare Range.compare h1 h2

let compare_env env1 env2 = IdMap.compare compare_av env1 env2

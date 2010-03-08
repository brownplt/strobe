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

  let add (lb1, ub1) (lb2, ub2) = 
    let lb = min lb1 lb2 in
    let ub = max ub1 ub2 in
    let lb = match lb with
      | Int n -> if n = 0 then Int 0 else if n > 0 then PosInf else NegInf
      | _ -> lb in
    let ub = match ub with
      | Int n -> if n = 0 then Int 0 else if n > 0 then PosInf else NegInf
      | _ -> ub in
      (lb, ub)


  let up v = match v with
    | Set s -> s
    | Range _ -> AVSet.singleton AV.ANumber

  let union v1 v2 = match v1, v2 with
    | Set s1, Set s2 -> Set (AVSet.union s1 s2)
    | Range (lb1, ub1), Range (lb2, ub2) -> Range (min lb1 lb2, max ub1 ub2)
    | _ -> Set (AVSet.union (up v1) (up v2))

  let intersect v1 v2 = match v1, v2 with
    | Range (lb1, ub1), Range (lb2, ub2) -> Range (max lb1 lb2, min ub1 ub2)
    | _ -> failwith "invalid args to intersect"


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

type heap = Range.t Heap.t

let deref loc heap =
  try 
    Heap.find loc heap
  with Not_found ->
    eprintf "%s is not a location in the heap " 
      (FormatExt.to_string Loc.pp loc);
    raise Not_found



module Type = struct

  type t =
    | Range of Range.t
    | LocTypeof of Loc.t
    | LocTypeIs of Loc.t * RTSet.t
    | LocRangeIs of Loc.t * Range.t
    | Deref of Loc.t

  let compare t1 t2 = match t1, t2 with
    | Range r1, Range r2 -> Range.compare r1 r2
    | LocTypeIs (l1, s1), LocTypeIs (l2, s2) when l1 = l2 -> RTSet.compare s1 s2
    | LocRangeIs (l1, r1), LocRangeIs (l2, r2) when l1 = l2 ->
        Range.compare r1 r2
    | _ -> Pervasives.compare t1 t2

  let up (h : heap) t : Range.t = match t with
    | Deref l -> deref l h
    | Range r -> r
    | LocTypeof _ -> Range.Set (AVSet.singleton AV.AString)
    | LocTypeIs _ -> Range.Set (AVSet.singleton AV.ABool)
    | LocRangeIs _ -> Range.Set (AVSet.singleton AV.ABool)

  let union h av1 av2 = match av1, av2 with
    | Range r1, Range r2 -> Range (Range.union r1 r2)
    | LocTypeof x, LocTypeof y when x = y -> LocTypeof x
    | LocTypeIs (x, s), LocTypeIs (y, t) when x = y -> 
        LocTypeIs (x, RTSet.union s t)
    | Deref x, Deref y when x = y -> Deref x
    | LocRangeIs (x, r1), LocRangeIs (y, r2) when x = y -> 
        LocRangeIs (x, Range.union r1 r2)
    | _ -> Range (Range.union (up h av1) (up h av2))



  open FormatExt

  let pp t = match t with
    | Range r -> Range.pp r
    | LocTypeof x -> sep [ text "typeof"; Loc.pp x ]
    | LocTypeIs (x, t) -> sep [ text "typeis";  Loc.pp x; 
                                 RTSetExt.p_set RT.pp t ]
    | LocRangeIs (x, r) ->
        sep [ Loc.pp x; text "in range"; Range.pp r ]
    | Deref l -> sep [ text "deref"; Loc.pp l ]

end


type env = Type.t IdMap.t

open FormatExt


let singleton t = Type.Range (Range.Set (AVSet.singleton t))

let empty = Type.Range (Range.Set AVSet.empty)

let union_env heap (env1 : env) (env2 : env) : env = 
  IdMapExt.join (Type.union heap) env1 env2

let p_env env = IdMapExt.p_map text Type.pp env

let p_heap heap = HeapExt.p_map Loc.pp Range.pp heap

let lookup (x : id) (env : env) =
  try
    IdMap.find x env
  with Not_found ->
    eprintf "%s is unbound in the environment:\n%s\n" x
    (FormatExt.to_string p_env env);
    raise Not_found

let bind (x : id) v  (env : env) : env = IdMap.add x v env

let empty_env = IdMap.empty

let union_heap h1 h2 = HeapExt.join Range.union h1 h2

let set_ref loc value heap =
  Heap.add loc value heap

let empty_heap = Heap.empty

let compare_heap h1 h2 = Heap.compare Range.compare h1 h2

let compare_env env1 env2 = IdMap.compare Type.compare env1 env2

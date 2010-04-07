open Prelude
open Typedjs_cps
open Typedjs_syntax

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

module Heap = Map.Make (Loc)
module HeapExt = MapExt.Make (Loc) (Heap)

type av =
  | ASet of RTSet.t
  | ADeref of Loc.t
  | ARef of Loc.t
  | ALocTypeof of Loc.t
  | ALocTypeIs of Loc.t * RTSet.t
  | AString of string
  | AClosure of int * id list * cpsexp

module AV = struct

  type t = av

  let compare v1 v2 = match v1, v2 with
    | ASet s1, ASet s2 -> RTSet.compare s1 s2
    | ALocTypeIs (l1, s1), ALocTypeIs (l2, s2) when l1 = l2 ->
        RTSet.compare s1 s2
    | _ -> Pervasives.compare v1 v2

end

type env = av IdMap.t

type heap = RTSet.t Heap.t

open FormatExt


let rec p_av av = match av with
  | ASet s -> RTSetExt.p_set RT.pp s
  | ADeref l -> horz [ text "deref"; Loc.pp l ]
  | ARef l -> horz [ text "ref"; Loc.pp l ]
  | ALocTypeof x -> horz [ text "typeof"; Loc.pp x ]
  | ALocTypeIs (x, t) -> 
      horz [ text "typeis"; Loc.pp x; RTSetExt.p_set RT.pp t ]
  | AClosure _ -> text "#closure"
  | AString s -> text ("\"" ^ s ^ "\"")

let singleton t = ASet (RTSet.singleton t)

let empty = ASet RTSet.empty

let rtany = 
  (RTSetExt.from_list
     [ RT.Number; RT.String; RT.Boolean; RT.Function; RT.Object; RT.Undefined ])

let any = ASet rtany

let p_heap h =
  HeapExt.p_map Loc.pp (RTSetExt.p_set RT.pp) h


let rec to_set heap v = match v with
  | ASet set -> set
  | ALocTypeof _ -> RTSet.singleton RT.String
  | ALocTypeIs _ -> RTSet.singleton RT.Boolean
  | AString _ -> RTSet.singleton RT.String
  | AClosure _ -> RTSet.singleton RT.Function
  | ARef _ -> rtany
  | ADeref loc -> 
      try Heap.find loc heap
      with Not_found ->
        eprintf "to_set cannot dereference location %s.\n"
          (FormatExt.to_string Loc.pp loc);
        raise Not_found

let deref loc heap  =
  try 
    Heap.find loc heap
  with Not_found ->
    eprintf "%s is not a location in the heap " (to_string Loc.pp loc);
    raise Not_found


let rec av_union h av1 av2 = match av1, av2 with
  | ASet s1, ASet s2 -> ASet (RTSet.union s1 s2)
  | ASet _, _ -> av_union h av1 (ASet (to_set h av2))
  | _, ASet _ -> av_union h (ASet (to_set h av1)) av2
  | ADeref x, ADeref y when x = y -> ADeref x
  | ARef x, ARef y when x = y -> ARef x
  | ALocTypeof x, ALocTypeof y when x = y -> ALocTypeof x
  | ALocTypeIs (x, s), ALocTypeIs (y, t) when x = y -> 
      ALocTypeIs (x, RTSet.union s t)
  | AString str1, AString str2 when str1 = str2 -> AString str1
  | AClosure (m, _, _), AClosure (n, _, _) -> 
      if m = n then av1
      else failwith "av_union on distinct closures"
  | _ -> av_union h (ASet (to_set h av1)) (ASet (to_set h av2))

let union_env h (env1 : env) (env2 : env) : env = 
  IdMapExt.join (fun _ -> av_union h) env1 env2

let p_env env = IdMapExt.p_map text p_av env

let lookup (x : id) (env : env) : av =
  try
    IdMap.find x env
  with Not_found ->
    eprintf "%s is unbound in the abstract environment" x;
    raise Not_found

let bind (x : id) (v : av) (env : env) : env = IdMap.add x v env

let empty_env = IdMap.empty

let escape_env (heap : heap) (env : env) : env =
  let f v = match v with
(*    | AClosure _ -> ASet (RTSet.singleton RT.Function) *)
    | ALocTypeIs _ -> ASet (RTSet.singleton RT.Boolean)
    | ALocTypeof _ -> ASet (RTSet.singleton RT.String)
    | _ -> v in
    IdMap.map f env

let escape_heap (heap : heap) = 
  Heap.map (fun _ -> rtany) heap

let set_ref loc value heap =
  Heap.add loc value heap

let rec rt_of_typ (t : Typedjs_syntax.typ) : RTSet.t = match t with
    Typedjs_syntax.TArrow _ -> RTSet.singleton RT.Function
  | Typedjs_syntax.TUnion (t1, t2) -> RTSet.union (rt_of_typ t1) (rt_of_typ t2)
  | Typedjs_syntax.TConstr (s, []) -> begin match s with
        "String" ->  RTSet.singleton RT.String
      | "RegExp" -> RTSet.singleton RT.Object
      | "Number"  -> RTSet.singleton RT.Number
      | "Int" -> RTSet.singleton RT.Number
      | "Boolean" -> RTSet.singleton RT.Boolean
      | "Undefined" -> RTSet.singleton RT.Undefined
      | _ -> RTSet.singleton RT.Object
    end
  | Typedjs_syntax.TConstr _ -> failwith 
      (sprintf "unknown type: %s" (to_string Typedjs_syntax.Pretty.p_typ t))
  | Typedjs_syntax.TObject _ -> RTSet.singleton RT.Object
  | Typedjs_syntax.TRef t -> rt_of_typ t
  | Typedjs_syntax.TSource t -> rt_of_typ t
  | Typedjs_syntax.TSink t -> rt_of_typ t
  | Typedjs_syntax.TTop -> rtany
  | Typedjs_syntax.TBot -> RTSet.empty
  | Typedjs_syntax.TForall _ -> rtany
  | Typedjs_syntax.TId _ -> rtany (* TODO: should be empty!!! *)

let runtime t : av = ASet (rt_of_typ t)

let union_heap h1 h2 =
  HeapExt.join (fun _ -> RTSet.union) h1 h2

let empty_heap = Heap.empty

let compare_heap = Heap.compare RTSet.compare

let compare_env = IdMap.compare AV.compare

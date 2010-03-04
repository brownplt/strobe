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

type env = av IdMap.t

type heap = av Heap.t

open FormatExt


let rec p_av av = match av with
  | ASet s -> RTSetExt.p_set RT.pp s
  | ADeref l -> horz [ text "deref"; Loc.pp l ]
  | ARef l -> horz [ text "ref"; Loc.pp l ]
  | ALocTypeof x -> horz [ text "typeof"; Loc.pp x ]
  | ALocTypeIs (x, t) -> 
      horz [ text "typeis"; Loc.pp x; RTSetExt.p_set RT.pp t ]
  | AClosure _ -> text "#closure"

let singleton t = ASet (RTSet.singleton t)

let empty = ASet RTSet.empty

let rtany = 
  (RTSetExt.from_list
     [ RT.Number; RT.String; RT.Boolean; RT.Function; RT.Object; RT.Undefined ])

let any = ASet rtany

let rec to_set heap v = match v with
  | ASet set -> set
  | ALocTypeof _ -> RTSet.singleton RT.String
  | ALocTypeIs _ -> RTSet.singleton RT.Boolean
  | AString _ -> RTSet.singleton RT.String
  | AClosure _ -> RTSet.singleton RT.Function
  | ARef _ -> rtany
  | ADeref loc -> to_set heap (Heap.find loc heap)

let deref loc heap : av =
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
  | ALocTypeof _, _ -> av_union h (singleton RT.String) av2
  | ALocTypeIs _, _ -> av_union h (singleton RT.Boolean) av2
  | _, ALocTypeof _ -> av_union h av1 (singleton RT.String)
  | _, ALocTypeIs _ -> av_union h av1 (singleton RT.Boolean)
  | ADeref _, _ -> av_union h (ASet (to_set h av1)) av2
  | _, ADeref _ -> av_union h av1 (ASet (to_set h av2))
  | AClosure (m, _, _), AClosure (n, _, _) when m = n -> av1

let union_env h (env1 : env) (env2 : env) : env = 
  IdMapExt.join (av_union h) env1 env2

let p_env env = IdMapExt.p_map text p_av env

let lookup (x : id) (env : env) : av =
  try
    IdMap.find x env
  with Not_found ->
    eprintf "%s is unbound in the abstract environment" x;
    raise Not_found

let bind (x : id) (v : av) (env : env) : env = IdMap.add x v env

let empty_env = IdMap.empty

let set_ref loc value heap =
  Heap.add loc value heap

let rec rt_of_typ (t : Typedjs_syntax.typ) : RTSet.t = match t with
    Typedjs_syntax.TArrow _ -> RTSet.singleton RT.Function
  | Typedjs_syntax.TUnion (t1, t2) -> RTSet.union (rt_of_typ t1) (rt_of_typ t2)
  | Typedjs_syntax.TApp (s, []) -> begin match s with
        "String" ->  RTSet.singleton RT.String
      | "RegExp" -> RTSet.singleton RT.Object
      | "Number"  -> RTSet.singleton RT.Number
      | "Int" -> RTSet.singleton RT.Number
      | "Boolean" -> RTSet.singleton RT.Boolean
      | "Undefined" -> RTSet.singleton RT.Undefined
      | _ -> failwith (sprintf "unknown type: TApp (\"%s\", [])" s)
    end
  | Typedjs_syntax.TApp _ -> failwith 
      (sprintf "unknown type: %s" 
         (pretty_string Typedjs_pretty.pretty_typ t))
  | Typedjs_syntax.TObject _ -> RTSet.singleton RT.Object
  | Typedjs_syntax.TRef t -> rt_of_typ t
  | Typedjs_syntax.TDom -> rtany
  | Typedjs_syntax.TTop -> rtany
  | Typedjs_syntax.TBot -> RTSet.empty

let runtime t : av = ASet (rt_of_typ t)

let union_heap h1 h2 =
  (* TODO: heap should just have RTSet.t *)
  HeapExt.join (av_union h1) h1 h2

let empty_heap = Heap.empty

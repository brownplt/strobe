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

let rec to_set heap v = match v with
  | ASet set -> set
  | ALocTypeof _ -> RTSet.singleton RT.String
  | ALocTypeIs _ -> RTSet.singleton RT.Boolean
  | AString _ -> RTSet.singleton RT.String
  | AClosure _ -> RTSet.singleton RT.Function
  | ARef _ -> rtany
  | ADeref loc -> Heap.find loc heap

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
      | _ -> RTSet.singleton RT.Object
    end
  | Typedjs_syntax.TApp _ -> failwith 
      (sprintf "unknown type: %s" 
         (pretty_string Typedjs_pretty.pretty_typ t))
  | Typedjs_syntax.TObject _ -> RTSet.singleton RT.Object
  | Typedjs_syntax.TRef t -> rt_of_typ t
  | Typedjs_syntax.TSource t -> rt_of_typ t
  | Typedjs_syntax.TSink t -> rt_of_typ t
  | Typedjs_syntax.TDom -> rtany
  | Typedjs_syntax.TTop -> rtany
  | Typedjs_syntax.TBot -> RTSet.empty

let runtime t : av = ASet (rt_of_typ t)

let union_heap h1 h2 =
  HeapExt.join RTSet.union h1 h2

let empty_heap = Heap.empty

let p_heap h =
  HeapExt.p_map Loc.pp (RTSetExt.p_set RT.pp) h

open Typedjs_syntax
open Typedjs_types


let compare_heap = Heap.compare RTSet.compare

let compare_env = IdMap.compare AV.compare

let rec simple_static cs lst  : typ = match lst with
  | [] -> TBot
  | RT.Number :: lst' ->typ_union cs typ_num (simple_static cs lst')
  | RT.String :: lst' ->typ_union cs typ_str (simple_static cs lst')
  | RT.Boolean :: lst' -> typ_union cs typ_bool (simple_static cs lst')
  | _ -> TDom

let rec static cs (rt : RTSet.t) (typ : typ) : typ = match typ with
  | TTop -> TTop
  | TBot -> TBot (* might change if we allow arbitrary casts *)
  | TArrow _ -> if RTSet.mem RT.Function rt then typ else TBot
  | TApp ("String", []) -> if RTSet.mem RT.String rt then typ else TBot
  | TApp ("RegExp", []) -> if RTSet.mem RT.Object rt then typ else TBot
  | TApp ("Number", []) -> if RTSet.mem RT.Number rt then typ else TBot
  | TApp ("Int", []) -> if RTSet.mem RT.Number rt then typ else TBot
  | TApp ("Boolean", []) -> if RTSet.mem RT.Boolean rt then typ else TBot
  | TApp ("Undefined", []) -> if RTSet.mem RT.Undefined rt then typ else TBot
  (* any other app will be an object from a constructor *)
  | TApp _ -> if RTSet.mem RT.Object rt then typ else TBot
  | TObject _ -> if RTSet.mem RT.Object rt then typ else TBot
  | TRef t -> TRef t
  | TSource t -> TSource t
  | TSink t -> TSink t
  | TDom -> simple_static cs (RTSetExt.to_list rt)
  | TUnion (s, t) -> typ_union cs (static cs rt s) (static cs rt t)

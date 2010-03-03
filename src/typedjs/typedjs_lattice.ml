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
  | ATypeof of id
  | ATypeIs of id * RTSet.t
  | AString of string
  | AClosure of int * id list * cpsexp

type env = av IdMap.t

type heap = av Heap.t

open FormatExt


let rec p_av av = match av with
  | ASet s -> RTSetExt.p_set RT.pp s
  | ATypeof x -> horz [ text "typeof"; text x ]
  | ATypeIs (x, t) -> horz [ text "typeis";  text x; 
                               RTSetExt.p_set RT.pp t ]
  | AClosure _ -> text "#closure"

let singleton t = ASet (RTSet.singleton t)

let empty = ASet RTSet.empty

let rtany = 
  (RTSetExt.from_list
     [ RT.Number; RT.String; RT.Boolean; RT.Function; RT.Object; RT.Undefined ])

let any = ASet rtany

let to_set v = match v with
  | ASet set -> set
  | ATypeof _ -> RTSet.singleton RT.String
  | ATypeIs _ -> RTSet.singleton RT.Boolean
  | AString _ -> RTSet.singleton RT.String
  | AClosure _ -> RTSet.singleton RT.Function

let deref loc heap : av =
  try 
    Heap.find loc heap
  with Not_found ->
    eprintf "%s is not a location in the heap " (to_string Loc.pp loc);
    raise Not_found


let rec av_union av1 av2 = match av1, av2 with
  | ASet s1, ASet s2 -> ASet (RTSet.union s1 s2)
  | ASet _, _ -> av_union av1 (ASet (to_set av2))
  | _, ASet _ -> av_union (ASet (to_set av1)) av2
  | ATypeof x, ATypeof y when x = y -> ATypeof x
  | ATypeIs (x, s), ATypeIs (y, t) when x = y -> ATypeIs (x, RTSet.union s t)
  | ATypeof _, _ -> av_union (singleton RT.String) av2
  | ATypeIs _, _ -> av_union (singleton RT.Boolean) av2
  | _, ATypeof _ -> av_union av1 (singleton RT.String)
  | _, ATypeIs _ -> av_union av1 (singleton RT.Boolean)
  | AClosure (m, _, _), AClosure (n, _, _) when m = n -> av1



let union_env (env1 : env) (env2 : env) : env = 
  IdMapExt.join av_union  env1 env2

let p_env env = IdMapExt.p_map text p_av env


let lookup (x : id) (env : env) : av=
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
    

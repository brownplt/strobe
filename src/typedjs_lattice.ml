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
  | ADeref of Loc.t * RTSet.t
  | ARef of Loc.t
  | ALocTypeof of Loc.t
  | ALocTypeIs of Loc.t * RTSet.t
  | AInstanceof of Loc.t * string
  | AStr of string
  | ABool of bool
  | AClosure of int * id list * cpsexp
  | AField of Loc.t * string
  | AThisField of string

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
  | ADeref (l, v) -> horz [ text "deref"; Loc.pp l; RTSetExt.p_set RT.pp v ]
  | ARef l -> horz [ text "ref"; Loc.pp l ]
  | ALocTypeof x -> horz [ text "LocTypeof"; Loc.pp x ]
  | ALocTypeIs (x, t) -> 
      horz [ text "LocType"; Loc.pp x; RTSetExt.p_set RT.pp t ]
  | AInstanceof (loc, constr_name) ->
    horz [ text "Instanceof"; Loc.pp loc; text constr_name ]
  | AClosure _ -> text "#closure"
  | AStr s -> text ("\"" ^ s ^ "\"")
  | ABool b -> text (string_of_bool b)
  | AField (loc, field_name) ->
    parens (horz [ text "get-field"; 
                   parens (horz [ text "deref"; Loc.pp loc ]);
                   text field_name ])

let singleton t = ASet (RTSet.singleton t)

let empty = ASet RTSet.empty

let rtany = 
  (RTSetExt.from_list
     [ RT.Num; RT.Str; RT.Bool; RT.Function; (RT.Object []); RT.Undefined ])

let any = ASet rtany

let p_heap h =
  HeapExt.p_map Loc.pp (RTSetExt.p_set RT.pp) h


let rec to_set v = match v with
  | ASet set -> set
  | ALocTypeof _ -> RTSet.singleton RT.Str
  | ALocTypeIs _ -> RTSet.singleton RT.Bool
  | AInstanceof _ -> RTSet.singleton RT.Bool
  | AStr _ -> RTSet.singleton RT.Str
  | AClosure _ -> RTSet.singleton RT.Function
  | ARef _ -> rtany
  | ABool _ -> RTSet.singleton RT.Bool
  | ADeref (_, v) -> v
  | AField _ -> rtany
  | AThisField _ -> rtany

let deref loc heap  =
  try 
    Heap.find loc heap
  with Not_found ->
    eprintf "%s is not a location in the heap " (to_string Loc.pp loc);
    raise Not_found


let rec av_union av1 av2 = match av1, av2 with
  | ASet s1, ASet s2 -> ASet (RTSet.union s1 s2)
  | ASet _, _ -> av_union av1 (ASet (to_set av2))
  | _, ASet _ -> av_union (ASet (to_set av1)) av2
  | ADeref (x ,s1), ADeref (y, s2) when x = y && RTSet.equal s1 s2 -> 
      ADeref (x, s1)
  | ARef x, ARef y when x = y -> ARef x
  | ALocTypeof x, ALocTypeof y when x = y -> ALocTypeof x
  | ALocTypeIs (x, s), ALocTypeIs (y, t) when x = y -> 
      ALocTypeIs (x, RTSet.union s t)
  | AInstanceof (loc1, constr_name1), AInstanceof (loc2, constr_name2) 
    when loc1 = loc2 && constr_name1 = constr_name2 ->
    AInstanceof (loc1, constr_name1)
  | AStr str1, AStr str2 when str1 = str2 -> AStr str1
  | AClosure (m, _, _), AClosure (n, _, _) -> 
      if m = n then av1
      else failwith "av_union on distinct closures"
  | ABool b1, ABool b2 when b1 = b2 -> ABool b1
  | AField (loc1, field_name1), AField (loc2, field_name2) when
      loc1 = loc2 && field_name1 = field_name2 -> av1
  | _ -> av_union (ASet (to_set av1)) (ASet (to_set av2))

let union_env = IdMapExt.join (fun _ -> av_union)

let p_env env = IdMapExt.p_map text p_av 
  (IdMapExt.filter (fun _ v -> match v with
                      |  ASet _ -> false
                      | _ -> true) 
     env)

let lookup (x : id) (env : env) : av =
  try
    IdMap.find x env
  with Not_found ->
    eprintf "%s is unbound in the abstract environment" x;
    raise Not_found

let bind (x : id) (v : av) (env : env) : env = 
  IdMap.add x v env

let empty_env = IdMap.empty

let escape_env (heap : heap) (env : env) : env =
  let f v = match v with
(*    | AClosure _ -> ASet (RTSet.singleton RT.Function) *)
    | ALocTypeIs _ -> ASet (RTSet.singleton RT.Bool)
    | ALocTypeof _ -> ASet (RTSet.singleton RT.Str)
    | AInstanceof _ -> ASet (RTSet.singleton RT.Bool)
    | AField _ -> ASet rtany
    | _ -> v in
    IdMap.map f env

let escape_heap (heap : heap) = 
  Heap.map (fun _ -> rtany) heap

let set_ref loc value heap =
  Heap.add loc value heap

let rec rt_of_typ (t : Typedjs_syntax.typ) : RTSet.t = match t with
    Typedjs_syntax.TArrow _ -> RTSet.singleton RT.Function
  | Typedjs_syntax.TRec (x, t') -> 
      rt_of_typ (Typedjs_syntax.Typ.typ_subst x t t') 
  | Typedjs_syntax.TUnion (t1, t2) -> RTSet.union (rt_of_typ t1) (rt_of_typ t2)
  | Typedjs_syntax.TConstr (constr_name, []) -> begin match constr_name with
        "Str" ->  RTSet.singleton RT.Str
      | "RegExp" -> RTSet.singleton (RT.Object [])
      | "Num"  -> RTSet.singleton RT.Num
      | "Int" -> RTSet.singleton RT.Num
      | "Bool" -> RTSet.singleton RT.Bool
      | "Undef" -> RTSet.singleton RT.Undefined
      |  _ -> RTSet.singleton (RT.ConstrObj constr_name)
    end
  | Typedjs_syntax.TConstr ("Array", [arrayt]) ->
    RTSet.singleton (RT.ConstrObj "Array")
  | Typedjs_syntax.TConstr _ -> failwith 
      (sprintf "unknown type: %s" (to_string Typedjs_syntax.Pretty.p_typ t))
  | Typedjs_syntax.TObject _ -> RTSet.singleton (RT.Object [])
  | Typedjs_syntax.TObjStar _ -> 
      RTSet.add (RT.Object []) (RTSet.singleton RT.Function)
  | Typedjs_syntax.TRef t -> rt_of_typ t
  | Typedjs_syntax.TSource t -> rt_of_typ t
  | Typedjs_syntax.TSink t -> rt_of_typ t
  | Typedjs_syntax.TTop -> rtany
  | Typedjs_syntax.TBot -> RTSet.empty
  | Typedjs_syntax.TForall _ -> rtany
  | Typedjs_syntax.TId _ -> rtany (* TODO: should be empty!!! *)
  | Typedjs_syntax.TField -> rtany

let runtime t : av = ASet (rt_of_typ t)

let union_heap h1 h2 =
  HeapExt.join (fun _ -> RTSet.union) h1 h2

let empty_heap = Heap.empty

let compare_heap = Heap.compare RTSet.compare

let compare_env = IdMap.compare AV.compare

let df_func_of_typ (t : typ) : av list -> av = match t with
  | TArrow (_, _, r_typ) ->
      let r_av = ASet (rt_of_typ r_typ) in
        (fun _ -> r_av)
  | _ -> (fun _ -> ASet rtany)

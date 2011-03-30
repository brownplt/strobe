open Prelude
open Sb_semicps
open Typedjs_syntax
open RegLang_syntax

module H = Hashtbl
module J = JavaScript_syntax

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

module Heap : sig
  type t

  val compare : t -> t -> int
  val empty : t
  val union : t -> t -> t
  val set_ref : loc -> RTSet.t -> t -> t
  val deref : loc -> t -> RTSet.t

end = struct
    
  module Map = Map.Make (Loc)
  module MapExt = MapExt.Make (Loc) (Map)

  type t = RTSet.t Map.t

  let compare = Map.compare RTSet.compare

  let empty = Map.empty

  let union h1 h2 = MapExt.join (fun _ -> RTSet.union) h1 h2

  let set_ref loc value heap = Map.add loc value heap

  let deref loc heap = try Map.find loc heap
    with Not_found ->
      let s = sprintf "%s is not a location in the heap " 
        (FormatExt.to_string Loc.pp loc) in
      raise (Invalid_argument s)

end

type heap = Heap.t

let rtany = 
  RTSetExt.from_list
    [ RT.Num; RT.Re any_str; RT.Bool; RT.Function; RT.Object; RT.Undefined ]

module Absval : sig
  type t = 
    | Set of RTSet.t
    | Deref of Loc.t * RTSet.t
    | Ref of Loc.t
        (* This value contains the string at the given location *)
    | LocStrAt of Loc.t * strpos
        (* This value represents a test against a constant string *)
    | LocStrAtIs of Loc.t * strpos * string
    | LocTypeof of Loc.t
    | LocTypeIs of Loc.t * RTSet.t
    | Val of cpsval
  and strpos =
    | Start
    | End

  val any : t
  val compare : t -> t -> int
  val union : t -> t -> t
  val to_set : t -> RTSet.t
  val mk_type_is : loc -> string -> t
  val mk_type_is_not : loc -> string -> t
  val mk_startswith : loc -> strpos -> string -> RT.t
        
end = struct
  
  type t = 
    | Set of RTSet.t
    | Deref of Loc.t * RTSet.t
    | Ref of Loc.t
        (* This value contains the string at the given location *)
    | LocStrAt of Loc.t * strpos
        (* This value represents a test against a constant string *)
    | LocStrAtIs of Loc.t * strpos * string
    | LocTypeof of Loc.t
    | LocTypeIs of Loc.t * RTSet.t
    | Val of cpsval
  and strpos =
    | Start
    | End

  let any = Set rtany

  let compare v1 v2 = match (v1, v2) with
    | Set s1, Set s2 -> RTSet.compare s1 s2
    | LocTypeIs (l1, s1), LocTypeIs (l2, s2) when l1 = l2 ->
      RTSet.compare s1 s2
    | _ -> Pervasives.compare v1 v1

  let to_set : t -> RTSet.t = function
    | Set set -> set
    | Deref (_, v) -> v
    | Ref _ -> rtany
    | LocStrAt _ -> RTSet.singleton (RT.Re any_str)
    | LocStrAtIs _ -> RTSet.singleton RT.Bool
    | LocTypeof _ -> RTSet.singleton (RT.Re any_str)
    | LocTypeIs _ -> RTSet.singleton RT.Bool
    | Val v -> match v with
        (* [lookup] should ensure that this does not occur *)
        | Id (_, x) -> failwith ("to_set applied to identifier " ^ x)
        | ExternalLambda _
        | Lambda _ -> RTSet.singleton RT.Function
        | Const c ->
          let open JavaScript_syntax in
          match c with
            | CBool _ -> RTSet.singleton RT.Bool
            | CString _ -> RTSet.singleton (RT.Re any_str)
            | CNull
            | CRegexp _ -> RTSet.singleton RT.Object
            | CNum _ 
            | CInt _ -> RTSet.singleton RT.Num
            | CUndefined -> RTSet.singleton RT.Undefined

  let rec union v1 v2 = match (v1, v2) with
    | Set s1, Set s2 -> Set (RTSet.union s1 s2)
    | Set _, _ -> union v1 (Set (to_set v2))
    | _, Set _ -> union (Set (to_set v1)) v2
    | Deref (x ,s1), Deref (y, s2) when x = y && RTSet.equal s1 s2 -> 
      Deref (x, s1)
    | Ref x, Ref y when x = y -> Ref x
    | LocTypeof x, LocTypeof y when x = y -> LocTypeof x
    | LocTypeIs (x, s), LocTypeIs (y, t) when x = y -> 
      LocTypeIs (x, RTSet.union s t)
    | (Val a, Val b) when a = b -> Val a
    | _ -> union (Set (to_set v1)) (Set (to_set v2))

  let tag_of_string = function
    | "string" -> RT.Re any_str
    | "number" -> RT.Num
    | "boolean" -> RT.Bool
    | "function" ->  RT.Function
    | "object" -> RT.Object
    | "undefined" ->  RT.Undefined
    | _ -> raise (Invalid_argument "tag_of_string")

  let mk_type_is (x : loc) (s : string) : t = 
    try 
      LocTypeIs (x, RTSet.singleton (tag_of_string s))
    with Invalid_argument "tag_of_string" ->
      Set (RTSet.singleton RT.Bool)

  let mk_type_is_not (x : loc) (s : string) : t =
    try 
      LocTypeIs (x, RTSet.remove (tag_of_string s) rtany)
    with Invalid_argument "tag_of_string" ->
      Set (RTSet.singleton RT.Bool)

  let mk_startswith x posn str = match posn with
    | Start -> 
      RT.Re (RegLang_syntax.Concat
               (RegLang_syntax.String str, RegLang_syntax.any_str))
    | End -> 
      RT.Re (RegLang_syntax.Concat
               (RegLang_syntax.any_str, RegLang_syntax.String str))

end

module Env : sig
  type t
  val empty : t
  val compare : t -> t -> int
  val union : t -> t -> t
  val lookup : id -> t -> Absval.t
  val is_bound : id -> t -> bool
  val is_owned : id -> t -> bool
  val bind : id -> Absval.t -> t -> t
  val from_typ_env : Typedjs_env.Env.env -> IdSet.t -> t
end = struct

  type t = {
    binds : Absval.t IdMap.t;
    owned : IdSet.t
  }

  let empty = { binds = IdMap.empty; owned = IdSet.empty }

  let compare env1 env2 = IdMap.compare Absval.compare env1.binds env2.binds

  let union env1 env2 = 
    { owned = IdSet.union env1.owned env2.owned;
      binds = IdMapExt.join (fun _ -> Absval.union) env1.binds env2.binds
    }

  let is_bound x env = IdMap.mem x env.binds

  let is_owned x env = IdSet.mem x env.owned

  let lookup x env = 
    try IdMap.find x env.binds
    (* Let the type-checker report unbound identifier errors. It can distinguish
       free variables from free labels, etc. *)
    with Not_found -> Absval.any

  let bind x v env = { env with binds = IdMap.add x v env.binds }

  module TypEnv = Typedjs_env.Env

  let rec rt_of_typ syns (t : Typedjs_syntax.typ) : RTSet.t =
    let open Typedjs_syntax in
    let rt = rt_of_typ syns in
    match t with
      | TArrow _ -> RTSet.singleton RT.Function
      | TUnion (t1, t2) -> RTSet.union (rt t1) (rt t2)
      | TIntersect (t1, t2) -> RTSet.union (rt t1) (rt t2)
      | TPrim (s) -> begin match s with
          | Str ->  RTSet.singleton (RT.Re any_str)
          | Num
          | Int -> RTSet.singleton RT.Num
          | True
          | False -> RTSet.singleton RT.Bool
          | Null -> RTSet.singleton RT.Object
          | Undef -> RTSet.singleton RT.Undefined
      end
      | TRegex (re, fsm) -> RTSet.singleton (RT.Re re)
      | TObject _ -> RTSet.singleton RT.Object
      | TRef t -> rt t
      | TSource t -> rt t
      | TSink t -> rt t
      | TTop -> rtany
      | TBot -> RTSet.empty
      | TForall _ -> rtany
      | TId _ -> rtany (* TODO: should be empty!!! *)
      | TField -> rtany
      | TRec (_, t) -> rt t
      | TSyn x -> rt (IdMap.find x syns)
      | TApp (t1, t2) -> rtany

  let runtime syns t : Absval.t = Absval.Set (rt_of_typ syns t)

  let from_typ_env typ_env owned =
    { binds = 
        IdMap.mapi (fun _ t -> runtime (TypEnv.syns typ_env) t) 
          (TypEnv.id_env typ_env);
      owned = owned }

end

type env = Env.t

let op_env = ref IdMap.empty
(*
let set_op_env env = 
  op_env := Typedjs_env.operator_env_of_tc_env env
*)


let calc_op1 node env heap (op : op1) v = match op, v with
  | Ref, v -> 
    let loc = Loc node in
    (Absval.Ref loc, Heap.set_ref loc (Absval.to_set v) heap)
  | Deref, Absval.Ref loc -> (Absval.Deref (loc, Heap.deref loc heap), heap)
  | Op1Prefix "prefix:typeof", Absval.Deref (loc, _) ->
    (Absval.LocTypeof loc, heap)
  | Op1Prefix "%ToBoolean", Absval.LocTypeIs _ -> (v, heap)
  | Op1Prefix "%ToBoolean", Absval.LocStrAtIs _ -> (v, heap)
  | _ -> (Absval.Set rtany, heap)

let calc_op2 x node env heap op v1 v2 = 
  let open Absval in
  let open JavaScript_syntax in
  match op, v1, v2 with
    | Op2Infix "charAt", Absval.Deref (loc, _), Val (Const (CInt 0)) ->
      (LocStrAt (loc, Start), heap)
        (** Insert something sensible here --- convert ending string to re *)
    | Op2Infix "===", LocStrAt (loc, posn), Val (Const (CString str)) ->
      (LocStrAtIs (loc, posn, str), heap)
    | Op2Infix "==", LocTypeof loc, Val (Const (CString str)) (* subtyping! *)
    | Op2Infix "===", LocTypeof loc, Val (Const (CString str))
    | Op2Infix "==", Val (Const (CString str)), LocTypeof loc
    | Op2Infix "===", Val (Const (CString str)), LocTypeof loc ->
      (mk_type_is loc str, heap)
    | Op2Infix "!=", LocTypeof loc, Val (Const (CString str))
    | Op2Infix "!==", LocTypeof loc, Val (Const (CString str))
    | Op2Infix "!=", Val (Const (CString str)), LocTypeof loc
    | Op2Infix "!==",  Val (Const (CString str)), LocTypeof loc ->
      (mk_type_is_not loc str, heap)
    | Op2Infix "+", _, _ -> 
      (Set (RTSetExt.from_list [ RT.Num; RT.Re any_str ]), heap)
    | Op2Infix op, v1, v2 -> (Set rtany, heap)
(* TODO: reintroduce
      begin 
        try
          let fn = IdMap.find op !op_env in
          (fn [ v1; v2 ], heap)
        with Not_found ->
          failwith (sprintf "operator %s is unbound in calc_op2" op)
      end
*)
    | _ -> (Set rtany, heap)

let bound_id_map : (pos * id, node) H.t = H.create 200

let lookup (v : cpsval) (node : node) (env : env) : Absval.t = match v with
  | Id (p, x) -> 
    H.replace bound_id_map (p, x) node;
    Env.lookup x env
  | _ -> Absval.Val v

(* The abstract environment at each node lets us lookup the abstract value
   of bound identifiers and perform abstract operations. *)
let envs : (node, env) H.t = H.create 200

let heaps : (node, heap) H.t = H.create 200

let reached_nodes : (node, bool) H.t = H.create 200
  
let rec calc (env : env) (heap : heap) (cpsexp : cpsexp) = match cpsexp with
  | Bind (node, x, bindexp, cont) ->
    let (v, heap) = match bindexp with
      | Let v -> (lookup v node env, heap)
      | Op1 (op, v) -> calc_op1 node env heap op (lookup v node env)
      | Op2 (SetRef, ((Id (_, x)) as v1), v2) ->
        begin match (lookup v1 node env), (lookup v2 node env) with
          | Absval.Ref l, v ->
            (v, if Env.is_owned x env 
              then Heap.set_ref l (Absval.to_set v) heap 
              else heap)
          | _, v -> (Absval.Set (Absval.to_set v), heap)
        end
      | Op2 (op, v1, v2) -> calc_op2 x node env heap op
        (lookup v1 node env) (lookup v2 node env)
      | Object _ -> (Absval.Set (RTSet.singleton RT.Object), heap)
      | Array _ -> (Absval.Set (RTSet.singleton RT.Object), heap)
      | UpdateField _ -> (Absval.Set (RTSet.singleton RT.Object), heap) in
    flow (Env.bind x v env) heap cont
  | Rec (node, binds, cont) ->
    let env = 
      fold_right (fun (x, v) env -> Env.bind x (Absval.Val v) env) binds env in
    flow env heap cont
  | If (node, v1, true_cont, false_cont) ->
    let absv1 = lookup v1 node env in
    let heap2, heap3, split, true_set, false_set = match absv1 with
      | Absval.LocTypeIs (loc, true_set) ->
        let false_set = RTSet.diff (Heap.deref loc heap) true_set in
        (Heap.set_ref loc true_set heap, Heap.set_ref loc false_set heap,
         true, true_set, false_set)
      | Absval.LocStrAtIs (loc, posn, str) ->
        let true_set = RTSet.singleton (Absval.mk_startswith loc posn str) in
        (Heap.set_ref loc true_set heap, heap, true, true_set, RTSet.empty)
      | _ -> 
        (heap, heap, false, RTSet.empty, RTSet.empty) in
    flow env heap2 true_cont;
    flow env heap3 false_cont
  | App (node, f, args) ->
    begin match lookup f node env with
      | Absval.Val (Lambda (xs, body)) ->
        let env = List.fold_right2 Env.bind
          xs (map (fun x -> lookup x node env) args)
          env in
        flow env heap body
      | _ -> ()
    end

and flow (env : env) (heap : heap) (cpsexp : cpsexp) = 
  let node = node_of_cpsexp cpsexp in
  let old_env, reflow  = 
    try H.find envs node, false
    with Not_found -> Env.empty, true in
  let old_heap, reflow =
    try H.find heaps node, reflow 
    with Not_found -> Heap.empty, true in
  let new_heap = Heap.union old_heap heap in
  let new_env = Env.union old_env env in
    if (Env.compare old_env new_env != 0 || 
        Heap.compare old_heap new_heap != 0 ||
        reflow) then
      begin
        H.replace reached_nodes node true;
        H.replace envs node new_env;
        H.replace heaps node new_heap;
        calc new_env new_heap cpsexp
      end
    else ()

module Annotate = struct
  open Typedjs_syntax

  let rec a_exp (exp : exp) : exp = match exp with
    | EConst _ -> exp
    | EBot _ -> exp
    | EAssertTyp (p, t, e) -> EAssertTyp (p, t, a_exp e)
    | EEmptyArray _ -> exp
    | EArray (p, es) -> EArray (p, map a_exp es)
    | EObject (p, props) -> EObject (p, map (second2 a_exp) props)
    | ESetRef (p', EId (p, x), e) -> begin 
      try
        let node = H.find bound_id_map (p, x) in
        let env = H.find envs node in
        let heap = H.find heaps node in
        match Env.lookup x env with
          | Absval.Ref loc -> 
            ESetRef (p', ETypecast (p, Heap.deref loc heap, EId (p, x)),
                     a_exp e)
          | _ -> ESetRef (p', EId (p, x), a_exp e)
      with Not_found -> 
        ESetRef (p', EId (p, x), a_exp e)
    end
    | EDeref (_, EId (p, x)) -> begin 
      try
        let node = H.find bound_id_map (p, x) in
        let env = H.find envs node in
        let heap = H.find heaps node in
        match Env.lookup x env with
          | Absval.Ref loc ->
            ETypecast (p, Heap.deref loc heap, exp)
          | _ -> exp
      with Not_found -> 
        exp 
    end
    | EId (p, x) -> begin 
      try 
        let node = H.find bound_id_map (p,x) in
        let env = H.find envs node in
        ETypecast (p, Absval.to_set (Env.lookup x env), exp)
      with Not_found -> exp 
    end
    | EBracket (p, e1, e2) -> EBracket (p, a_exp e1, a_exp e2)
    | EUpdate (p, e1, e2, e3) -> EUpdate (p, a_exp e1, a_exp e2, a_exp e3)
    | ENew (p, c_id, es) -> ENew (p, c_id, map a_exp es)
    | EPrefixOp (p, op, e) -> EPrefixOp (p, op, a_exp e)
    | EInfixOp (p, op, e1, e2) -> EInfixOp (p, op, a_exp e1, a_exp e2)
    | EIf (p, e1, e2, e3) -> EIf (p, a_exp e1, a_exp e2, a_exp e3)
    | EApp (p, e1, es) -> EApp (p, a_exp e1, map a_exp es)
    | EFunc (p, ids, t, e) -> exp (* no descent into functions *)
    | ELet (p, x, e1, e2) -> ELet (p, x, a_exp e1, a_exp e2)
    | ERec (binds, e) -> ERec (map a_bind binds, a_exp e)
    | ESeq (p, e1, e2) -> ESeq (p, a_exp e1, a_exp e2)
    | ELabel (p, i, t, e) -> ELabel (p, i, t, a_exp e)
    | EBreak (p, i, e) -> EBreak (p, i, a_exp e)
    | ETryCatch (p, e1, i, e2) -> ETryCatch (p, a_exp e1, i, a_exp e2)
    | ETryFinally (p, e1, e2) -> ETryFinally (p, a_exp e1, a_exp e2)
    | EThrow (p, e) -> EThrow (p, a_exp e)
    | ETypecast (p, t, e) -> ETypecast (p, t, a_exp e)
    | ERef (p, k, e) -> ERef (p, k, a_exp e)
    | EDeref (p, e) -> EDeref (p, a_exp e)
    | ESetRef (p, e1, e2) -> ESetRef (p, a_exp e1, a_exp e2)
    | ESubsumption (p, t, e) -> ESubsumption (p, t, a_exp e)
    | EDowncast (p, t, e) -> EDowncast (p, t, a_exp e)
    | ETypAbs (p, x, t, e) -> ETypAbs (p, x, t, a_exp e)
    | ETypApp (p, e, t) -> ETypApp (p, a_exp e, t)
    | ECheat (p, t, e) -> ECheat (p, t, a_exp e)
    | EForInIdx _ -> exp
      
  and a_bind (i, t, e) = (i, t, a_exp e)

end

let semicfa owned_ids typ_env exp =
  let env = Env.from_typ_env typ_env owned_ids in
  let open Sb_semicps in
  let cpsexp = cps_exp exp "#exn" (Jmp "#ret") in
  (* TBot is the wrong type *)
  let env = Env.bind "#ret" (Absval.Val (ExternalLambda Typedjs_syntax.TBot)) 
    env in
  let env = Env.bind "#exn" (Absval.Val (ExternalLambda Typedjs_syntax.TBot)) 
    env in
  flow env Heap.empty cpsexp;
  Annotate.a_exp exp



open Prelude
open Typedjs_syntax

exception Not_wf_typ of string

(* Necessary for equi-recursive subtyping. *)
module TypPair = struct
  type t = typ * typ
  let compare = Pervasives.compare
end

module TPSet = Set.Make (TypPair)
module TPSetExt = SetExt.Make (TPSet)

module Env = struct

  type class_info = {
    fields : typ IdMap.t;
    sup : constr option
  }

  type env = {
    id_typs : typ IdMap.t; 
    lbl_typs : typ IdMap.t;
    classes : class_info IdMap.t;
    subclasses : id IdMap.t; (* direct subclasses *)
    typ_ids: typ IdMap.t; (* bounded type variables *)
    typ_syns : typ IdMap.t; (* type synonyms *)
  }


  let empty_env = { 
    id_typs = IdMap.empty;
    lbl_typs = IdMap.empty;
    classes = IdMap.empty;
    subclasses = IdMap.empty;
    typ_ids = IdMap.empty;
    typ_syns = IdMap.empty;
  }

  let bind_id x t env  = { env with id_typs = IdMap.add x t env.id_typs }

  let bind_lbl x t env = { env with lbl_typs = IdMap.add x t env.lbl_typs }

  let bind_typ_id x t env = { env with typ_ids = IdMap.add x t env.typ_ids }

  let lookup_id x env = IdMap.find x env.id_typs

  let lookup_lbl x env = IdMap.find x env.lbl_typs

  let rec field_typ env cname fname = try
    let ci = IdMap.find cname env.classes in
      if IdMap.mem fname ci.fields then
        Some (IdMap.find fname ci.fields)
      else begin match ci.sup with
        | None -> None
        | Some cname' -> field_typ env cname' fname
      end
  with Not_found -> raise (Not_wf_typ ("undefined class: " ^ cname))

  let is_class env cname = IdMap.mem cname env.classes

  let id_env env = env.id_typs

  let clear_labels env = { env with lbl_typs = 
      try 
        IdMap.add "%return" (IdMap.find "%return" env.lbl_typs) IdMap.empty
      with Not_found -> IdMap.empty }

  let dom env = IdSetExt.from_list (IdMapExt.keys env.id_typs)

  exception Not_subtype

  let rec subt env cache s t = 
    if TPSet.mem (s, t) cache then
      cache
    else if s = t then
      cache
    else
      let subtype = subt env in
      let cache = TPSet.add (s, t) cache in
      match s, t with
        | TSyn x, _ -> subt env cache (IdMap.find x env.typ_syns) t
        | _, TSyn y -> subt env cache s (IdMap.find y env.typ_syns)
        | TPrim Int, TPrim Num -> cache
        | TRegex (_, fsm1), TRegex (_, fsm2) ->
            if RegLang.contains fsm1 fsm2 then cache 
            else raise Not_subtype
        | TRegex _, TPrim Str -> cache
        | TId x, TId y -> if x = y then cache else raise Not_subtype
        | TId x, t ->
          let s = IdMap.find x env.typ_ids in (* S-TVar *)
            subtype cache s t (* S-Trans *)
        | TIntersect (s1, s2), _ -> 
          begin 
            try subtype cache s1 t
            with Not_subtype -> subtype cache s2 t
          end
        | _, TIntersect (t1, t2) ->
            subt env (subt env cache s t1) s t2
        | TUnion (s1, s2), _ -> subt env (subt env cache s1 t) s2 t
        | _, TUnion (t1, t2) ->
          begin 
            try subtype cache s t1
            with Not_subtype -> subtype cache s t2
          end
       | TArrow (args1, r1), TArrow (args2, r2) ->
          begin
            try List.fold_left2 subtype cache (r1 :: args2) (r2 :: args1)
            with Invalid_argument _ -> raise Not_subtype (* unequal lengths *)
          end
        | TObject fs1, TObject fs2 -> subtype_fields env cache fs1 fs2
        | TRef s', TRef t' -> subtype (subtype cache s' t') t' s'
        | TSource s, TSource t -> subtype cache s t
        | TSink s, TSink t -> subtype cache t s
        | TRef s, TSource t -> subtype cache s t
        | TRef s, TSink t -> subtype cache t s
        | _, TTop -> cache
        | TBot, _ -> cache
        | _ -> raise Not_subtype

  (* assumes fs1 and fs2 are ordered 
     fs1 <: fs2 if fs1 has everything fs2 does, and maybe more *)
  and subtype_fields env cache fs1 fs2 = match fs1, fs2 with
    | [], [] -> cache
    | [], _ -> raise Not_subtype (* fs1 is missing some things fs2 has *)
    | _, [] -> cache (* can have many extra fields, doesn't matter *)
    | (x, s) :: fs1', (y, t) :: fs2' -> raise Not_subtype

  let subtypes env ss ts = 
    try 
      let _ = List.fold_left2 (subt env) TPSet.empty ss ts in
      true
    with 
      | Invalid_argument _ -> false (* unequal lengths *)
      | Not_subtype -> false

  let subtype env s t = 
    try
      let _ = subt env TPSet.empty s t in
      true
    with Not_subtype -> false

  let typ_union cs s t = match subtype cs s t, subtype cs t s with
      true, true -> s (* t = s *)
    | true, false -> t (* s <: t *)
    | false, true -> s (* t <: s *)
    | false, false -> TUnion (s, t)

  let typ_intersect cs s t = match subtype cs s t, subtype cs t s with
    | true, true -> s
    | true, false -> s (* s <: t *)
    | false, true -> t
    | false, false -> TIntersect (s, t)

  let rec normalize_typ env typ = match typ with
    | TPrim _ -> typ
    | TUnion (s, t) -> 
        typ_union env (normalize_typ env s) (normalize_typ env t)
    | TIntersect (s, t) -> 
        typ_intersect env (normalize_typ env s) (normalize_typ env t)
    | TRegex _ -> typ
    | TObject fs ->
        TObject (map (second2 (normalize_typ env)) fs)
    | TArrow (args, result) ->
        TArrow (map (normalize_typ env) args,
                normalize_typ env result)
    | TRef t -> TRef (normalize_typ env t)
    | TSource t -> TSource (normalize_typ env t)
    | TSink t -> TSink (normalize_typ env t)
    | TTop -> TTop
    | TBot -> TBot
    | TField -> TField
    | TId x ->
        if IdMap.mem x env.typ_ids then typ
        else raise (Not_wf_typ ("the type variable " ^ x ^ " is unbound"))
    | TForall (x, s, t) -> 
        let s = normalize_typ env s in
          TForall (x, s, normalize_typ (bind_typ_id x s env) t)
    | TRec (x, t) ->
      TRec (x, normalize_typ (bind_typ_id x typ env) t)
    | TSyn x ->
      if IdMap.mem x env.typ_syns then typ
      else raise (Not_wf_typ (x ^ " is not a type"))

  let check_typ p env t = 
    try normalize_typ env t
    with Not_wf_typ s -> raise (Typ_error (p, s))

  let basic_static env (typ : typ) (rt : RT.t) : typ = match rt with
    | RT.Num -> typ_union env (TPrim Num) typ
    | RT.Str -> typ_union env (TPrim Str) typ
    | RT.Bool -> typ_union env typ_bool typ
    | RT.Function -> typ_union env TField typ
    | RT.Object -> typ_union env TField typ
    | RT.Undefined -> typ_union env (TPrim Undef) typ

  let basic_static2 env (typ : typ) (rt : RT.t) : typ = match rt with
    | RT.Num -> typ_union env (TPrim Num) typ
    | RT.Str -> typ_union env (TPrim Str) typ
    | RT.Bool -> typ_union env typ_bool typ
    | RT.Function -> typ_union env (TObject []) typ
    | RT.Object -> typ_union env (TObject []) typ
    | RT.Undefined -> typ_union env (TPrim Undef) typ

  let rec static cs (rt : RTSet.t) (typ : typ) : typ = match typ with
    | TBot -> TBot (* might change if we allow arbitrary casts *)
    | TArrow _ -> if RTSet.mem RT.Function rt then typ else TBot
    | TPrim (Str)
    | TRegex _ -> if RTSet.mem RT.Str rt then typ else TBot
    | TPrim (Num) 
    | TPrim (Int) -> if RTSet.mem RT.Num rt then typ else TBot
    | TPrim (True)
    | TPrim (False) -> if RTSet.mem RT.Bool rt then typ else TBot
    | TPrim (Null) -> if RTSet.mem RT.Object rt then typ else TBot
    | TPrim (Undef) -> 
        if RTSet.mem RT.Undefined rt then typ else TBot
          (* any other app will be an object from a constructor *)
    | TObject _ -> if RTSet.mem RT.Object rt then typ else TBot
    | TRef t -> TRef t
    | TSource t -> TSource t
    | TSink t -> TSink t
    | TUnion (s, t) -> typ_union cs (static cs rt s) (static cs rt t)
    | TIntersect (s, t) -> typ_intersect cs (static cs rt s) (static cs rt t)
    | TForall _ -> typ
    | TField -> List.fold_left (basic_static cs) TBot (RTSetExt.to_list rt)
    | TTop -> 
        if RTSet.equal rt Typedjs_lattice.rtany then
          TTop
        else (* TODO: no arrow type that is the supertype of all arrows *)
          List.fold_left (basic_static2 cs) TBot (RTSetExt.to_list rt)
    | TId _ -> typ
    | TRec (x, t) -> TRec (x, static cs rt t)
    | TSyn _ -> typ


  let new_root_class env class_name = 
    if IdMap.mem class_name env.classes then
      raise (Invalid_argument ("class already exists: " ^ class_name))
    else 
      let c = IdMap.add class_name 
        { fields = IdMap.empty; sup = None } env.classes in
        { env with
            classes = c
        }

  let new_subclass env sub_name sup_name =
    { env with
        classes = IdMap.add sub_name
        { fields = IdMap.empty; sup = Some sup_name } env.classes;
        subclasses = IdMap.add sub_name sup_name env.subclasses }

  let add_method c_name m_name m_typ env =
    let ci = IdMap.find c_name env.classes in
      if IdMap.mem m_name ci.fields then
        raise (Invalid_argument ("method already exists: " ^ m_name))
      else 
        let ci' = { ci with fields = IdMap.add m_name m_typ ci.fields } in
          { env with classes = IdMap.add c_name ci' env.classes }

  let rec set_global_object env cname =
    let ci = IdMap.find cname env.typ_syns in
    match ci with
      | TObject fs ->
        let add_field env (x, t) = begin match x with
          | (RegLang.String s, _) -> bind_id s t env 
          | _ -> raise (Not_wf_typ (cname ^ " field was a regex in global"))
        end in
        List.fold_left add_field env fs
      | _ -> 
        raise (Not_wf_typ (cname ^ " global must be an object"))

  let rec bind_typ env typ : env * typ = match typ with
    | TForall (x, s, t) -> bind_typ (bind_typ_id x s env) t
    | typ -> (env, typ)

  let syns env = env.typ_syns

end

open Lexing

let parse_env (cin : in_channel) (name : string) : env_decl list =
  let lexbuf = Lexing.from_channel cin in
    try
      lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with 
                                      Lexing.pos_fname = name };
      Typedjs_parser.env Typedjs_lexer.token lexbuf
    with
      | Failure "lexing: empty token" ->
          failwith (sprintf "error lexing environment at %s"
                      (string_of_position 
                         (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))
      | Typedjs_parser.Error ->
          failwith (sprintf "error parsing environment at %s"
                      (string_of_position 
                         (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))


let extend_global_env env lst =
  let add env decl = match decl with
    | EnvBind (x, typ) ->
      if IdMap.mem x env.Env.id_typs then
        raise (Not_wf_typ (x ^ " is already bound in the environment"))
      else
        Env.bind_id x typ env
    | EnvClass (x, _, t) ->
      if IdMap.mem x env.Env.typ_syns then
        raise (Not_wf_typ ("the type " ^ x ^ " is already defined"))
      else
        { env with Env.typ_syns = IdMap.add x t env.Env.typ_syns }
  in List.fold_left add env lst

module L = Typedjs_lattice

let df_func_of_typ syns (t : typ) : L.av list -> L.av = match t with
  | TArrow (_, r_typ) ->
      let r_av = L.ASet (L.rt_of_typ syns r_typ) in
        (fun _ -> r_av)
  | TForall (x, r_typ, TArrow (_, TId y)) when x = y ->
      let r_av = L.ASet (L.rt_of_typ syns r_typ) in
        (fun _ -> r_av)
  | _ -> (fun _ -> L.any)

let cf_env_of_tc_env tc_env = 
  let fn x typ cf_env = L.bind x (L.runtime tc_env.Env.typ_syns typ) cf_env in
    IdMap.fold fn (Env.id_env tc_env) L.empty_env

let operator_env_of_tc_env tc_env =
  let fn x t env = IdMap.add x (df_func_of_typ (Env.syns tc_env) t) env in
    IdMap.fold fn (Env.id_env tc_env) IdMap.empty
  

let rec typ_subst x s typ = match typ with
  | TPrim _ -> typ
  | TId y -> if x = y then s else typ
  | TUnion (t1, t2) -> TUnion (typ_subst x s t1, typ_subst x s t2)
  | TIntersect (t1, t2) ->
      TIntersect (typ_subst x s t1, typ_subst x s t2)
  | TArrow (t2s, t3)  ->
      TArrow (map (typ_subst x s) t2s, typ_subst x s t3)
  | TObject fs -> TObject (map (second2 (typ_subst x s)) fs)
  | TRef t -> TRef (typ_subst x s t)
  | TSource t -> TSource (typ_subst x s t)
  | TSink t -> TSink (typ_subst x s t)
  | TTop -> TTop
  | TBot -> TBot
  | TField -> TField
  | TForall (y, t1, t2) -> 
      if x = y then 
        TForall (y, typ_subst x s t1, t2)
      else 
        failwith "TODO: capture-free substitution"
  | TRec (y, t) ->
    if x = y then
      typ
    else 
      failwith "TODO: capture-free substitution"

let typ_unfold typ = match typ with
    | TRec (x, t) -> typ_subst x typ t
    | _ -> typ

let simpl_typ env typ = 
  let typ = match typ with
    | TSyn x -> IdMap.find x env.Env.typ_syns (* normalization => success *)
    | _ -> typ
  in typ_unfold typ


let apply_subst subst typ = IdMap.fold typ_subst subst typ

 (* TODO: occurs check *)
let rec unify subst s t : typ IdMap.t = match s, t with
  | TPrim s, TPrim t -> 
      if s = t then subst 
      else failwith ("cannot unify differently typed primitives")
  | TId x, TId y -> 
      if x = y then subst 
      else failwith ("cannot unify bound variables " ^ x ^ " and " ^ y)
  | TId x, t -> 
      if IdMap.mem x subst then
        begin
          let s = IdMap.find x subst in
            IdMap.add x (apply_subst subst (TUnion (s, t))) subst
        end
      else
        IdMap.add x (apply_subst subst t) subst
  | s, TId y -> unify subst (TId y) s
  | TUnion (s1, s2), TUnion (t1, t2) -> unify (unify subst s1 t1) s2 t2
  | TIntersect (s1, s2), TIntersect (t1, t2) -> 
      unify (unify subst s1 t1) s2 t2
  | TArrow (s2s, s3), TArrow (t2s, t3) ->
      List.fold_left2 unify subst (s3 :: s2s) (t3 :: t2s)
  | TObject fs1, TObject fs2 ->
      let f subst (x, s) (y, t) = 
        if x = y then unify subst s t
        else failwith "cannot unify objects with distinct field names" in
      List.fold_left2 f subst fs1 fs2
  | TRef s, TRef t -> unify subst s t
  | TSource s, TSource t -> unify subst s t
  | TSink s, TSink t -> unify subst s t
  | TTop, TTop -> subst
  | TBot, TBot -> subst
  | TForall _, TForall _ -> failwith "cannot unify quantified types"
  | _ -> failwith ("unification failure" ^ 
                   (Typedjs_syntax.string_of_typ s) ^ " " ^ 
                   (Typedjs_syntax.string_of_typ t))


let unify_typ (s : typ) (t : typ) : typ IdMap.t = 
  unify IdMap.empty s t

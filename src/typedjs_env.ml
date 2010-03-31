open Prelude
open Typedjs_syntax

exception Not_wf_typ of string

module IdPairSet = Set.Make 
  (struct
     type t = id * id
     let compare = Pervasives.compare
   end)

module Env = struct

  type env = {
    id_typs : typ IdMap.t; 
    lbl_typs : typ IdMap.t;
    (* maps class names to a structural object type *)
    classes : typ IdMap.t;
    (* reflexive-transitive closure of the subclass relation *)
    subclasses : IdPairSet.t
  }


  let empty_env = { 
    id_typs = IdMap.empty;
    lbl_typs = IdMap.empty;
    classes = IdMap.empty;
    subclasses = IdPairSet.empty
  }

  let bind_id x t env  = { env with id_typs = IdMap.add x t env.id_typs }

  let bind_lbl x t env = { env with lbl_typs = IdMap.add x t env.lbl_typs }

  let lookup_id x env = IdMap.find x env.id_typs

  let lookup_lbl x env = IdMap.find x env.lbl_typs

  let lookup_class x env = 
    try 
      IdMap.find x env.classes
    with Not_found -> 
      eprintf "class %s does not exist\n" x;
      raise Not_found

  let id_env env = env.id_typs

  let get_classes env = env.classes (* ocaml sucks *)

  let clear_labels env = { env with lbl_typs = IdMap.empty }

  let dom env = IdSetExt.from_list (IdMapExt.keys env.id_typs)

  let rec subtype env s t = 
    let subtype = subtype env in
    let subtypes = subtypes env in
      match s, t with
        | TConstr (c1, []), TConstr (c2, []) ->
            IdPairSet.mem  (c1, c2) env.subclasses
        | TConstr (c1, args1), TConstr (c2, args2) ->
            if c1 = c2 then subtypes args1 args2 else false
        | TUnion (s1, s2), _ -> 
            subtype s1 t && subtype s2 t
        | _, TUnion (t1, t2) ->
            subtype s t1 || subtype s t2
        | TArrow (_, args1, r1), TArrow (_, args2, r2) ->
            subtypes args2 args1 && subtype r1 r2
        | TObject fs1, TObject fs2 -> subtype_fields env fs1 fs2
            (* objects vs. constructed objects: *)
        | TObject fs1, TConstr (cid, apps) -> begin try
            match IdMap.find cid env.classes with
                TObject fs2 -> subtype_fields env fs1 fs2
          with
              _ -> false
          end
        | TConstr (cid, apps), TObject fs2 -> begin try
            match IdMap.find cid env.classes with
                TObject fs1 -> subtype_fields env fs1 fs2
          with
              _ -> false
          end
            (* this will handle inheritance as well: *)
        | TConstr (cid1, apps1), TConstr (cid2, apps2) -> begin try
            match IdMap.find cid1 env.classes, IdMap.find cid2 env.classes with
                TObject fs1, TObject fs2 -> subtype_fields env fs1 fs2
          with
              _ -> false
          end
        | TRef s', TRef t' -> subtype s' t' && subtype t' s'
        | TSource s, TSource t -> subtype s t
        | TSink s, TSink t -> subtype t s
        | TRef s, TSource t -> subtype s t
        | TRef s, TSink t -> subtype t s
        | _, TTop -> true
        | TBot, _ -> true
        | _ -> s = t

  (* assumes fs1 and fs2 are ordered *)
  and subtype_fields env fs1 fs2 = match fs1, fs2 with
    | [], [] -> true
    | [], _ -> true
    | _, [] -> false (* fs1 has fields that fs2 does not *)
    | (x, s) :: fs1', (y, t) :: fs2' ->
        let cmp = String.compare x y in
          if cmp = 0 then subtype env s t && subtype_fields env fs1' fs2'
          else if cmp < 0 then false (* we will not find x in the supertype *)
            (* y is an extra field in the supertype *)
          else subtype_fields env fs1 fs2' 

  and subtypes env (ss : typ list) (ts : typ list) : bool = 
    try List.for_all2 (subtype env) ss ts
    with Invalid_argument _ -> false (* unequal lengths *)

  let typ_union cs s t = match subtype cs s t, subtype cs t s with
      true, true -> s (* t = s *)
    | true, false -> t (* s <: t *)
    | false, true -> s (* t <: s *)
    | false, false -> TUnion (s, t)

  let cmp_props (k1, _) (k2, _) = match String.compare k1 k2 with
    | 0 -> raise (Not_wf_typ ("the field " ^ k1 ^ " is repeated"))
    | n -> n
        
  let rec normalize_typ env typ = match typ with
    | TUnion (s, t) -> 
        typ_union env (normalize_typ env s) (normalize_typ env t)
    | TObject fs ->
        let fs = List.fast_sort cmp_props fs in
          TObject (map (second2 (normalize_typ env)) fs)
    | TConstr ("Array", [t]) -> TConstr ("Array", [normalize_typ env t])
    | TConstr (constr, []) ->
        if IdMap.mem constr env.classes then typ
        else raise (Not_wf_typ (constr ^ " is not a type constructor"))
    | TConstr (constr, _) ->
        raise (Not_wf_typ (constr ^ " does not take arguments"))
    | TArrow (this, args, result) ->
        TArrow (normalize_typ env this, map (normalize_typ env) args,
                normalize_typ env result)
    | TRef t -> TRef (normalize_typ env t)
    | TSource t -> TSource (normalize_typ env t)
    | TSink t -> TSink (normalize_typ env t)
    | TTop -> TTop
    | TBot -> TBot

  let check_typ p env t = 
    try normalize_typ env t 
    with Not_wf_typ s -> raise (Typ_error (p, s))

  let rec static cs (rt : RTSet.t) (typ : typ) : typ = match typ with
    | TTop -> TTop
    | TBot -> TBot (* might change if we allow arbitrary casts *)
    | TArrow _ -> if RTSet.mem RT.Function rt then typ else TBot
    | TConstr ("String", []) -> if RTSet.mem RT.String rt then typ else TBot
    | TConstr ("RegExp", []) -> if RTSet.mem RT.Object rt then typ else TBot
    | TConstr ("Number", []) -> if RTSet.mem RT.Number rt then typ else TBot
    | TConstr ("Int", []) -> if RTSet.mem RT.Number rt then typ else TBot
    | TConstr ("Boolean", []) -> if RTSet.mem RT.Boolean rt then typ else TBot
    | TConstr ("Undefined", []) -> 
        if RTSet.mem RT.Undefined rt then typ else TBot
          (* any other app will be an object from a constructor *)
    | TConstr _ -> if RTSet.mem RT.Object rt then typ else TBot
    | TObject _ -> if RTSet.mem RT.Object rt then typ else TBot
    | TRef t -> TRef t
    | TSource t -> TSource t
    | TSink t -> TSink t
    | TUnion (s, t) -> typ_union cs (static cs rt s) (static cs rt t)


  let new_class class_name env = 
    if IdMap.mem class_name env.classes then
      raise (Invalid_argument ("class already exists: " ^ class_name))
    else 
      { env with
          classes = IdMap.add class_name (TObject []) env.classes;
          subclasses = IdPairSet.add (class_name, class_name) env.subclasses
      }

  let new_subclass env sub_name sup_name =
    { env with
        subclasses = IdPairSet.add (sub_name, sup_name) env.subclasses }

  let add_method class_name method_name method_typ env =
    let class_typ = IdMap.find class_name env.classes in
      match class_typ with
          TObject fields ->
            if List.mem_assoc method_name fields then
              raise (Invalid_argument ("method already exists: " ^ method_name))
            else
              let class_typ' = TObject ((method_name, method_typ) :: fields) in
                { env with classes = IdMap.add class_name 
                    (normalize_typ env class_typ') env.classes }
        | _ ->
            failwith ("class type is not an object: " ^ class_name)

  let set_global_object env cname = match lookup_class cname env with
    | TObject fs -> 
        List.fold_left (fun env (x, t) -> bind_id x t env) env fs
    | _ -> failwith "set_global_object: got a class that is not an object"



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


let rec add_methods (lst : (id * typ) list) (class_name : id) (env : Env.env) = 
  match lst with
      [] -> env
    | (method_name, method_typ) :: rest ->
        add_methods rest class_name
          (Env.add_method class_name method_name method_typ env)

let rec add_classes (lst : env_decl list) (env : Env.env) = match lst with
  | [] -> env
  | EnvClass (cname, _, _) :: rest -> add_classes rest (Env.new_class cname env)
  | _ :: rest -> add_classes rest env

(* [mk_env'] ensures that a type declaration is  well-formed and well-kinded.
   For these checks, it needs the existing environment. *)
let rec mk_env' (lst : env_decl list) (env : Env.env) : Env.env =  
  match lst with
    | [] -> env
    | EnvBind (x, typ) :: rest ->
        if IdMap.mem x env.Env.id_typs then
          raise (Not_wf_typ (x ^ " is already bound in the environment"))
        else
          mk_env' rest (Env.bind_id x typ env)
    | EnvClass (class_name, proto, methods) :: rest ->
        let env = try match proto with
          | None -> add_methods methods class_name env
          | Some proto_name ->
              add_methods methods class_name
                (Env.new_subclass env proto_name class_name)
        with Not_wf_typ s ->
          raise (Not_wf_typ ("error adding class " ^ class_name ^ "; " ^ s)) in
          (* TODO account for prototype *)
          mk_env' rest env

let extend_global_env env lst = mk_env' lst (add_classes lst env)

module L = Typedjs_lattice

let cf_env_of_tc_env tc_env = 
  let fn x typ cf_env = L.bind x (L.runtime typ) cf_env in
    IdMap.fold fn (Env.id_env tc_env) L.empty_env

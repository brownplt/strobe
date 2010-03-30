open Prelude
open Typedjs_syntax

exception Not_wf_typ of string

module Env = struct

  type env = { id_typs : typ IdMap.t; 
               lbl_typs : typ IdMap.t;
               (* maps class names to a structural object type *)
               classes : typ IdMap.t 
             }


  let empty_env = { 
    id_typs = IdMap.empty;
    lbl_typs = IdMap.empty;
    classes = IdMap.empty
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

  let cmp_props (k1, _) (k2, _) = match String.compare k1 k2 with
    | 0 -> raise (Not_wf_typ ("the field " ^ k1 ^ " is repeated"))
    | n -> n

        
  let rec normalize_typ env typ = match typ with
    | TUnion (s, t) -> 
        Typedjs_types.typ_union env.classes
          (normalize_typ env s) (normalize_typ env t)
    | TObject fs ->
        let fs = List.fast_sort cmp_props fs in
          TObject (map (second2 (normalize_typ env)) fs)
    | TApp ("Array", [t]) -> TApp ("Array", [normalize_typ env t])
    | TApp (constr, []) ->
        if IdMap.mem constr env.classes then typ
        else raise (Not_wf_typ (constr ^ " is not a type constructor"))
    | TApp (constr, _) ->
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


  let new_class class_name env = 
    if IdMap.mem class_name env.classes then
      raise (Invalid_argument ("class already exists: " ^ class_name))
    else 
      { env with
          classes = IdMap.add class_name (TObject []) env.classes
      }


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
      [] -> env
    | EnvBind (x, typ) :: rest ->
        if IdMap.mem x env.Env.id_typs then
          raise (Not_wf_typ (x ^ " is already bound in the environment"))
        else
          mk_env' rest (Env.bind_id x typ env)
    | EnvClass (class_name, proto_typ, methods) :: rest ->
        let env = try 
          add_methods methods class_name env 
        with Not_wf_typ s ->
          raise (Not_wf_typ ("error adding class " ^ class_name ^ "; " ^ s)) in
          (* TODO account for prototype *)
          mk_env' rest env

let extend_global_env env lst = mk_env' lst (add_classes lst env)

module L = Typedjs_lattice

let cf_env_of_tc_env tc_env = 
  let fn x typ cf_env = L.bind x (L.runtime typ) cf_env in
    IdMap.fold fn (Env.id_env tc_env) L.empty_env

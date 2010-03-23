open Prelude
open Typedjs_syntax

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

  let lookup_class x env = IdMap.find x env.classes

  let id_env env = env.id_typs

  let get_classes env = env.classes (* ocaml sucks *)

  let clear_labels env = { env with lbl_typs = IdMap.empty }

  let dom env = IdSetExt.from_list (IdMapExt.keys env.id_typs)

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
                    (Typedjs_types.typ_permute class_typ') env.classes }
        | _ ->
            failwith ("class type is not an object: " ^ class_name)

  let union env1 env2 = 
    let err k _ _ = 
      failwith (k ^ " is defined multiple times in the environment") in
      { id_typs = IdMapExt.join err env1.id_typs env2.id_typs;
        lbl_typs = IdMapExt.join err env1.lbl_typs env2.lbl_typs;
        classes = IdMapExt.join err env1.classes env2.classes
      }

end

let parse_env (cin : in_channel) (name : string) : env_decl list =
  let lexbuf = Lexing.from_channel cin in
    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with 
                                   Lexing.pos_fname = name };
    Typedjs_parser.env Typedjs_lexer.token lexbuf

let rec add_methods (lst : (id * typ) list) (class_name : id) (env : Env.env) = 
  match lst with
      [] -> env
    | (method_name, method_typ) :: rest ->
        add_methods rest class_name
          (Env.add_method class_name method_name method_typ env)


(* [mk_env'] ensures that a type declaration is  well-formed and well-kinded.
   For these checks, it needs the existing environment. *)
let rec mk_env' (lst : env_decl list) (env : Env.env) : Env.env =  
  match lst with
      [] -> env
    | EnvBind (x, typ) :: rest ->
        mk_env' rest (Env.bind_id x typ env)
    | EnvClass (class_name, proto_typ, methods) :: rest ->
        let env = Env.new_class class_name env in
        let env = add_methods methods class_name env in
          (* TODO account for prototype *)
          mk_env' rest env

let mk_env (lst : env_decl list) : Env.env = mk_env' lst Env.empty_env

module L = Typedjs_lattice

let cf_env_of_tc_env tc_env = 
  let fn x typ cf_env = L.bind x (L.runtime typ) cf_env in
    IdMap.fold fn (Env.id_env tc_env) L.empty_env

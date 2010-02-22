open Prelude
open Typedjs_syntax

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

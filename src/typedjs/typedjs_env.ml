open Prelude
open Typedjs_syntax

let parse_env (cin : in_channel) (name : string) : env_decl list =
  let lexbuf = Lexing.from_channel cin in
    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with 
                                   Lexing.pos_fname = name };
    Typedjs_parser.env Typedjs_lexer.token lexbuf

let rec mk_env (lst : env_decl list) : Env.env = match lst with
    [] -> Env.empty_env
  | EnvBind (x, typ) :: rest ->
      Env.bind_id x typ (mk_env rest)


open Prelude
open Lambdajs_syntax
open Lexing

type parsed_env = (id * exp) list

let parse_env cin name : parsed_env =
  let lexbuf = Lexing.from_channel cin in
    try 
      (* Set the correct filename in lexbuf (for source-tracking). *)
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      Lambdajs_parser.env Lambdajs_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "lexical error at %s"
                       (string_of_position 
                          (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))
      | Lambdajs_parser.Error ->
           failwith (sprintf "error parsing environment at %s"
                       (string_of_position 
                          (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))


(* All names "x" in the environment are renamed to "[[x]]" *)
let rename_env (env : parsed_env) : parsed_env  =
  let mk_name x = "[[" ^ x ^ "]]" in
  let ns = map fst2 env in
  let rename_rhs exp = fold_left (fun e x -> rename x (mk_name x) e) exp ns in
  let rename_bind (x, exp) = (mk_name x, rename_rhs exp) in
    map rename_bind env

let enclose_in_env (env : parsed_env) (exp : exp) : exp =
  let env' = rename_env env in
    EFix ((dummy_pos, dummy_pos), env', exp)

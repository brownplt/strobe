open Format
module Lexer = Idl_lexer
module Parser = Idl_parser
open Idl_syntax



let from_channel (cin : in_channel) (src_name : string) =
  let open Lexing in
  let lexbuf = from_channel cin in
  try 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = src_name };
      Parser.idlFile Lexer.token lexbuf
  with
  | Failure "lexing: empty token" ->
    failwith (sprintf "%s:%d:%d lexical error" src_name 
                 lexbuf.lex_curr_p.pos_lnum
                (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))
  | Parser.Error ->
    failwith (sprintf "%s:%d:%d parse error, unexpected token %s" src_name
                lexbuf.lex_curr_p.pos_lnum
                (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
                (lexeme lexbuf))

open Prelude
open JavaScript_syntax
open Lexing

let parse_javascript cin name =
  let lexbuf = Lexing.from_channel cin in
    try 
      (* Set the correct filename in lexbuf (for source-tracking). *)
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      JavaScript_parser.program JavaScript_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "lexical error at %s"
                       (string_of_position 
                          (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))
      | JavaScript_parser.Error ->
           failwith (sprintf "parse error at %s"
                       (string_of_position 
                          (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))

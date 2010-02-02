open Prelude
open JavaScript_syntax
open Lexing


let parse_javascript cin name =
  JavaScript_lexer.comments := [];
  let lexbuf = Lexing.from_channel cin in
    (* Set the correct filename in lexbuf (for source-tracking). *)
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
    let prog = JavaScript_parser.program JavaScript_lexer.token lexbuf in
    let comments = !JavaScript_lexer.comments in
      JavaScript_lexer.comments := [];
      (prog, comments)

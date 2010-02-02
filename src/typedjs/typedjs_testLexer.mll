{
  open Prelude
  open Lexing
  open Typedjs_syntax

  type expected =
      ExpectedTyp of typ
    | ExpectedFails

  type test =
      pos * (pos JavaScript_syntax.expr) * (pos * string) list * expected

  let expr_buf = Buffer.create 120

  let expr_start_p = ref dummy_pos

  let expected_buf = Buffer.create 120

  let tests : test list ref = ref []

  let new_test (expected : expected) : unit =
    let expr_lexbuf = from_string (Buffer.contents expr_buf) in
      Buffer.clear expr_buf;
      expr_lexbuf.lex_start_p <- !expr_start_p;
      expr_lexbuf.lex_curr_p <- !expr_start_p;
      let expr = 
        JavaScript_parser.expression JavaScript_lexer.token expr_lexbuf
      and comments = !JavaScript_lexer.comments in
        JavaScript_lexer.comments := [];
        tests := ((!expr_start_p, !expr_start_p), expr, comments, expected) ::
          !tests

}

let ident = ['a'-'z' 'A'-'Z' '$' '_']['a'-'z' 'A'-'Z' '0'-'9' '$' '_']*

let blank = [ ' ' '\t' '\r' ]

rule expr = parse
  | eof { 
      let not_ws = ref false in
      let check ch = 
        if ch == ' ' || ch = '\t' || ch == '\n' || ch == '\t' then ()
        else  not_ws := true in
        String.iter check (Buffer.contents expr_buf);
        if !not_ws then
          (failwith (sprintf "%s: trailing characters:\n%s" 
                       lexbuf.lex_curr_p.pos_fname (Buffer.contents expr_buf)))
    }
  | "@@" { expected lexbuf }
  | [^ '@' '\n' '\r' ]+ as str { Buffer.add_string expr_buf str; expr lexbuf }
  | ['\r' '\n'] { new_line lexbuf; Buffer.add_char expr_buf '\n'; expr lexbuf }
  | '@' { Buffer.add_char expr_buf '@'; expr lexbuf }

and ws = parse
  | ['\r' '\n'] { new_line lexbuf; ws lexbuf }
  | [' ' '\t']+ { ws lexbuf }
  | ';' { expr_start_p := lexeme_start_p lexbuf; expr lexbuf }

and expected = parse
  | "fails" {
      new_test ExpectedFails;
      ws lexbuf
    }
  | ([^ ';' '\r' '\n']+ as str) {
      let typ_lexbuf = from_string str in
        typ_lexbuf.lex_start_p <- lexeme_start_p lexbuf;
        typ_lexbuf.lex_curr_p <- lexeme_start_p lexbuf;
        let typ = Typedjs_parser.typ Typedjs_lexer.token typ_lexbuf in
          new_test (ExpectedTyp typ);
          ws lexbuf 
    }

          
{
  let parse_tests (cin : in_channel) (name : string) : test list =
    tests := [];
    let lexbuf = from_channel cin in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      lexbuf.lex_start_p <- lexbuf.lex_curr_p;
      expr_start_p := lexbuf.lex_start_p;
      expr lexbuf;
      !tests

}

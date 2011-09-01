open Prelude
open Lexing
open Jsdoc_syntax
open Jsdoc_parser
open Jsdoc_lexer

let parse_jsdoc (str : string) (p : position) : typ =
  let lexbuf = from_string str in
  try
    lexbuf.lex_curr_p <- p;
    annotation token lexbuf
  with Error ->
    failwith (sprintf "parse error at %s; unexpected token %s"
                (string_of_position (lexbuf.lex_curr_p, lexbuf.lex_curr_p))
                (lexeme lexbuf))

let comments : (pos * string) list ref = ref []

let jsdoc_of_comment ((p1, p2), str) = 
  if String.length str = 0 then
    None
  else if String.get str 0 = '*' then
    Some ((p1, p2), parse_jsdoc str p1)
  else
    None

let jsdoc_of_comments comments =
  let f c lst = match jsdoc_of_comment c with
    | None -> lst
    | Some v -> v :: lst in
  List.fold_right f comments []

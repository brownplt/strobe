{
open Lexing
open Jsdoc_parser

let depth = ref 0

let in_typ () = !depth > 0
}

let ident = ['a'-'z' 'A'-'Z' '$' '_']['a'-'z' 'A'-'Z' '0'-'9' '$' '_']*

rule token = parse
  | "@param" { PARAM }
  | "@return" { RETURN }
  | '{' { incr depth; LBRACE }
  | '}' { decr depth; RBRACE }
  | '.' { if in_typ () then DOT else token lexbuf }
  | '?' { if in_typ () then QUES else token lexbuf }
  | '!' { if in_typ () then BANG else token lexbuf }
  | "function" { if in_typ () then FUNCTION else token lexbuf }
  | '(' { if in_typ () then LPAREN else token lexbuf }
  | ')' { if in_typ () then RPAREN else token lexbuf }
  | ':' { if in_typ () then COLON else token lexbuf }
  | '<' { if in_typ () then LANGLE else token lexbuf }
  | '>' { if in_typ () then RANGLE else token lexbuf }
  | '|' { if in_typ () then PIPE else token lexbuf }
  | ',' { if in_typ () then COMMA else token lexbuf }
  | '*' { if in_typ () then ALL else token lexbuf }
  | ident as x { if in_typ () then ID x else token lexbuf }
  | _ { token lexbuf }
  | [ '\n' '\r' ] { new_line lexbuf; token lexbuf }
  | eof { EOF }

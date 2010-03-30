{
  open Prelude
  open Lexing
  open Typedjs_parser

}

let ident = ['a'-'z' 'A'-'Z' '$' '_']([ '.' 'a'-'z' 'A'-'Z' '0'-'9' '$' '_']*)

let blank = [ ' ' '\t' '\r' ]

(* NOTE: Do not use @@ or ; as tokens. They are used by the test-case parser,
   which is particularly naive. *)
rule token = parse
   | blank + { token lexbuf }
   | '\n' { new_line lexbuf; token lexbuf }
   | '\r' { new_line lexbuf; token lexbuf }
   | "\r\n" { new_line lexbuf; token lexbuf }
   | "/*" { block_comment lexbuf }
   | "//"[^ '\r' '\n']* [ '\r' '\n' ] { new_line lexbuf; token lexbuf }

   | "->" { ARROW }
   | "(" { LPAREN }
   | ")" { RPAREN }
   | "{" { LBRACE }
   | "}" { RBRACE }
   | "," { COMMA }
   | "Any" { ANY }
   | "Int" { INT }
   | "Double" { NUM }
   | "String" { STR }
   | "Bool" { BOOL }
   | "*" { STAR }
   | ":" { COLON }
   | "+" { UNION }
   | "Void" { UNDEF }
   | "constructor" { CONSTRUCTOR }
   | "function" { FUNCTION }
   | "prototype" { PROTOTYPE }
   | "class" { CLASS }
   | "upcast" { UPCAST }
   | "downcast" { DOWNCAST }
   | "val" { VAL }
   | "<" { LANGLE }
   | ">" { RANGLE }
   | eof { EOF }
   | ident as x { ID x }

and block_comment = parse
  | "*/" { token lexbuf }
  | '*' { block_comment lexbuf }
  | [ '\n' '\r' ]  { block_comment lexbuf }
  | [^ '\n' '\r' '*'] { block_comment lexbuf }

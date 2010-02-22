{
  open Prelude
  open Lexing
  open Typedjs_parser

}

let ident = ['a'-'z' 'A'-'Z' '$' '_']['a'-'z' 'A'-'Z' '0'-'9' '$' '_']*

let blank = [ ' ' '\t' '\r' ]

(* NOTE: Do not use @@ or ; as tokens. They are used by the test-case parser,
   which is particularly naive. *)
rule token = parse
   | blank + { token lexbuf }
   | '\n' { new_line lexbuf; token lexbuf }
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
   | "Dom" { DOM }
   | "*" { STAR }
   | "::" { COLONCOLON }
   | ":" { COLON }
   | "+" { UNION }
   | "Void" { UNDEF }
   | "constructor" { CONSTRUCTOR }
   | "mutable" { MUTABLE }
   | "function" { FUNCTION }
   | "prototype" { PROTOTYPE }
   | eof { EOF }
   | ident as x { ID x }

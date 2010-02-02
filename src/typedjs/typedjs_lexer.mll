{
  open Prelude
  open Lexing
  open Typedjs_parser

}

let ident = ['a'-'z' 'A'-'Z' '$' '_']['a'-'z' 'A'-'Z' '0'-'9' '$' '_']*

let blank = [ ' ' '\t' '\r' ]

rule token = parse
   | blank + { token lexbuf }
   | '\n' { new_line lexbuf; token lexbuf }
   | "->" { ARROW }
   | "(" { LPAREN }
   | ")" { RPAREN }
   | "Any" { ANY }
   | "Int" { INT }
   | "Double" { NUM }
   | "String" { STR }
   | "*" { STAR }
   | ":" { COLON }
   | "+" { UNION }
   | "Void" { UNDEF }
   | eof { EOF }
   | ident as x { ID x }

{
  open Lexing
  open Idl_parser

}

let ident = ['a'-'z' 'A'-'Z' '$' '_' '%']([ 'a'-'z' 'A'-'Z' '0'-'9' '$' '_']*)

let blank = [ ' ' '\t' '\r' ]

let hex = ['0'-'9' 'A'-'f' 'a'-'f']

let escape_sequence = [^ '\r' '\n'] | ('x' hex hex) | ('u' hex hex hex hex)

let double_quoted_string_char = [^ '\r' '\n' '"' '\\'] | ('\\' escape_sequence)


rule token = parse
   | blank + { token lexbuf }
   | '\n' { new_line lexbuf; token lexbuf }
   | '\r' { new_line lexbuf; token lexbuf }
   | "\r\n" { new_line lexbuf; token lexbuf }
   | "/*" { block_comment lexbuf }
   | "//"[^ '\r' '\n']* [ '\r' '\n' ] { new_line lexbuf; token lexbuf }
   | "(" { LPAREN }
   | ")" { RPAREN }
   | "{" { LBRACE }
   | "}" { RBRACE }
   | "[" { LBRACK }
   | "]" { RBRACK }
   | "[]" { LRBRACK }
   | "," { COMMA }
   | ":" { COLON }
   | "?" { QUES }
   | "module" { MODULE }
   | "in" { IN }
   | "interface" { INTERFACE }
   | ";" { SEMI }
   | "short" { SHORT }
   | "long long" { LONGLONG }
   | "long" { LONG }
   | "boolean" { BOOLEAN }
   | "byte" { BYTE }
   | "octet" { OCTET }
   | "float" { FLOAT }
   | "double" { DOUBLE }
   | "DOMString" { DOMSTRING }
   | "Date" { DATE }
   | "any" { ANY }
   | "unsigned" { UNSIGNED }
   | "readonly" { READONLY }
   | "void" { VOID }
   | "optional" { OPTIONAL }
   | "attribute" { ATTRIBUTE }
   | "typedef" { TYPEDEF }
   | "Object" { OBJECT }
   | "exception" { EXCEPTION }
   | "const" { CONST }
   | "=" { EQUALS }
   | "raises" { RAISES }
   | ['0'-'9']+ as x { INT (int_of_string x) }
   | "0x" hex+ { INT 0 }
   | eof { EOF }
   | ident as x { ID (Id.id_of_string x) }



and block_comment = parse
  | "*/" { token lexbuf }
  | '*' { block_comment lexbuf }
  | "\r\n" { new_line lexbuf; block_comment lexbuf }
  | [ '\n' '\r' ]  { new_line lexbuf; block_comment lexbuf }
  | [^ '\n' '\r' '*'] { block_comment lexbuf }


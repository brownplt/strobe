{
  open Prelude
  open Lexing
  open Typedjs_parser

}

let ident = ['a'-'z' 'A'-'Z' '$' '_']([ '.' 'a'-'z' 'A'-'Z' '0'-'9' '$' '_']*)

let blank = [ ' ' '\t' '\r' ]

let hex = ['0'-'9' 'A'-'f' 'a'-'f']

let escape_sequence = [^ '\r' '\n'] | ('x' hex hex) | ('u' hex hex hex hex)

let double_quoted_string_char = [^ '\r' '\n' '"' '\\'] | ('\\' escape_sequence)


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
   | "[" { LBRACK }
   | "]" { RBRACK }
   | "," { COMMA }
   | "Any" { ANY }
   | "Int" { INT }
   | "Double" { NUM }
   | "String" { STR }
   | "Bool" { BOOL }
   | "Undef" { UNDEF }
   | "*" { STAR }
   | ":" { COLON }
   | "+" { UNION }
   | "." { DOT }
   | "constructor" { CONSTRUCTOR }
   | "prototype" { PROTOTYPE }
   | "class" { CLASS }
   | "upcast" { UPCAST }
   | "downcast" { DOWNCAST }
   | "operator" { OPERATOR }
   | "is" { IS }
   | "val" { VAL }
   | "<" { LANGLE }
   | ">" { RANGLE }
   | "forall" { FORALL }
   | "<:" { LTCOLON }
   | eof { EOF }
   | ident as x { ID x }
   | '"' (double_quoted_string_char* as x) '"' { STRING x }
   | '\''(ident as x) { TID x }


and block_comment = parse
  | "*/" { token lexbuf }
  | '*' { block_comment lexbuf }
  | [ '\n' '\r' ]  { block_comment lexbuf }
  | [^ '\n' '\r' '*'] { block_comment lexbuf }

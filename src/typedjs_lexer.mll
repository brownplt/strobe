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
   | "Num" { NUM }
   | "Str" { STR }
   | "Bool" { BOOL }
   | "Undef" { UNDEF }
   | "Bot" { BOT }
   | "*" { STAR }
   | ":" { COLON }
   | "+" { UNION }
   | "." { DOT }
   | "..." { DOTS }
   | ";" { SEMI }
   | "constructor" { CONSTRUCTOR }
   | "prototype" { PROTOTYPE }
   | "#proto" { HASHPROTO }
   | "#code" { CODE }
   | "trec" { TREC }
   | "class" { CLASS }
   | "upcast" { UPCAST }
   | "downcast" { DOWNCAST }
   | "operator" { OPERATOR }
   | "ref" { REF }
   | "const" { CONST }
   | "is" { IS }
   | "obj*" { OBJCAST }
   | "fresh" { FRESH }
   | "cheat" { CHEAT }
   | "val" { VAL }
   | "<" { LANGLE }
   | ">" { RANGLE }
   | "forall" { FORALL }
   | "checked" { CHECKED }
   | "type" { TYPE }
   | "=" { EQUALS }
   | "<:" { LTCOLON }
   | "$" { CASH }
   | "^" { CARET }
   | "BAD" { BAD }
   | "_" { UNDER }
   | eof { EOF }
   | ident as x { ID x }
   | '"' (double_quoted_string_char* as x) '"' { STRING x }
   | '\''(ident as x) { TID x }


and block_comment = parse
  | "*/" { token lexbuf }
  | '*' { block_comment lexbuf }
  | [ '\n' '\r' ]  { block_comment lexbuf }
  | [^ '\n' '\r' '*'] { block_comment lexbuf }

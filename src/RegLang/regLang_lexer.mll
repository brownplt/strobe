{
  open Prelude
  open Lexing
  open RegLang_parser

}

let ch = ['a'-'z' 'A'-'Z' '0' - '9' '$' '_' '%']

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
   | "|" { PIPE }
   | "*" { STAR }
   | "." { DOT }
   | "<:" { LTCOLON }
   | "</:" { LTSLASHCOLON }
   | ";" { SEMI }
   | eof { EOF }
   | ch as x { CHAR x }
   | '\\' (ch as x) { CHAR x }
   | '"' (double_quoted_string_char* as x) '"' { STRING x }
   | '\''(ch as x) { CHAR x }


and block_comment = parse
  | "*/" { token lexbuf }
  | '*' { block_comment lexbuf }
  | [ '\n' '\r' ]  { block_comment lexbuf }
  | [^ '\n' '\r' '*'] { block_comment lexbuf }

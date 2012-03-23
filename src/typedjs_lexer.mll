{
  open Prelude
  open Lexing
  open Typedjs_parser

  let string_buf = Buffer.create 100

  let get_string () = 
    let s = Buffer.contents string_buf in
    Buffer.clear string_buf;
    s

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

   | "..." { DOTS }
   | "->" { ARROW }
   | "=>" { THICKARROW }
   | "(" { LPAREN }
   | ")" { RPAREN }
   | "#{" { HASHBRACE }
   | "{" { LBRACE }
   | "{{" { LLBRACE }
   | "}}" { RRBRACE }
   | "}" { RBRACE }
   | "[" { LBRACK }
   | "]" { RBRACK }
   | "<" { LANGLE }
   | ">" { RANGLE }
   | "," { COMMA }
   | ";" { SEMI }
   | "Any" { ANY }
   | "Str" { STR }
   | "Bool" { BOOL }
   | "Num" { PRIM "Num" }
   | "True" { PRIM "True" }
   | "False" { PRIM "False" }
   | "Undef" { PRIM "Undef" }
   | "Null" { PRIM "Null" }
   | "@" (ident as x) { PRIM x }
   | "_" { UNDERSCORE }
   | "BAD" { BAD }
   | "ref" { REF }
   | "*" { STAR }
   | ":" { COLON }
   | "::" { COLONCOLON }
   | "+" { UNION }
   | "&" { INTERSECTION }
   | "." { DOT }
   | "=" { EQUALS }
   | "upcast" { UPCAST }
   | "downcast" { DOWNCAST }
   | "operator" { OPERATOR }
   | "this" { THIS }
   | "is" { IS }
   | "cheat" { CHEAT }
   | "val" { VAL }
   | "forall" { FORALL }
   | "type" { TYPE }
   | "typlambda" { TYPLAMBDA }
   | "typrec" { TYPREC }
   | "<:" { LTCOLON }
   | "?" { QUES }
   | "^" { CARET }
   | "!" { BANG }
   | "rec" { REC }
   | "primitive" { PRIMITIVE }
   | "with" { WITH }
   | eof { EOF }
   | ident as x { ID x }
   | '"' (double_quoted_string_char* as x) '"' { STRING x }
   | '\''(ident as x) { TID x }
   | '/' { regexp lexbuf }



and block_comment = parse
  | "*/" { token lexbuf }
  | '*' { block_comment lexbuf }
  | "\r\n" { new_line lexbuf; block_comment lexbuf }
  | [ '\n' '\r' ]  { new_line lexbuf; block_comment lexbuf }
  | [^ '\n' '\r' '*'] { block_comment lexbuf }

and regexp = parse
  | "/" { REGEX (get_string ()) }
  | '\\' (_ as ch) { Buffer.add_char string_buf ch; regexp lexbuf }
  | _ as ch { Buffer.add_char string_buf ch; regexp lexbuf }

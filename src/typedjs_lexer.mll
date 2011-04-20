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
   | "#{" { HASHBRACE }
   | "{" { LBRACE }
   | "}" { RBRACE }
   | "[" { LBRACK }
   | "]" { RBRACK }
   | "<" { LANGLE }
   | ">" { RANGLE }
   | "," { COMMA }
   | "Any" { ANY }
   | "Int" { INT }
   | "Num" { NUM }
   | "Str" { STR }
   | "Bool" { BOOL }
   | "True" { TRUE }
   | "False" { FALSE }
   | "Undef" { UNDEF }
   | "Null" { NULL }
   | "_" { UNDERSCORE }
   | "BAD" { BAD }
   | "*" { STAR }
   | ":" { COLON }
   | "+" { UNION }
   | "&" { INTERSECTION }
   | "." { DOT }
   | "=" { EQUALS }
   | "constructor" { CONSTRUCTOR }
   | "upcast" { UPCAST }
   | "downcast" { DOWNCAST }
   | "operator" { OPERATOR }
   | "is" { IS }
   | "cheat" { CHEAT }
   | "val" { VAL }
   | "forall" { FORALL }
   | "type" { TYPE }
   | "<:" { LTCOLON }
   | "?" { QUES }
   | "!" { BANG }
   | "rec" { REC }
   | eof { EOF }
   | ident as x { ID x }
   | '"' (double_quoted_string_char* as x) '"' { STRING x }
   | '\''(ident as x) { TID x }
   | '/' { regexp lexbuf }



and block_comment = parse
  | "*/" { token lexbuf }
  | '*' { block_comment lexbuf }
  | [ '\n' '\r' ]  { block_comment lexbuf }
  | [^ '\n' '\r' '*'] { block_comment lexbuf }

and regexp = parse
  | "/" { REGEX (get_string ()) }
  | '\\' (_ as ch) { Buffer.add_char string_buf ch; regexp lexbuf }
  | _ as ch { Buffer.add_char string_buf ch; regexp lexbuf }

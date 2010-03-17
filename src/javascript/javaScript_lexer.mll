{
open Prelude
open Lexing
open JavaScript_parser
open JavaScript_syntax

module S = String

(* Requires: start < String.length str *)
let rec drop_spaces (str : string) (start : int) = 
  match String.get str start with
    ' '  -> drop_spaces str (start + 1)
  | '\t' -> drop_spaces str (start + 1)
  | '\r' -> drop_spaces str (start + 1)
  | '\n' -> drop_spaces str (start + 1)
  |  _   -> String.sub str start (String.length str - start)


(* TODO: if integer conversions overflow, treat as a float *)
let parse_num_lit (s : string) (l : pos) : token =
  if S.contains s 'x' || S.contains s 'X'
    then Int (l,int_of_string s)
    else if S.contains s '.'
           then Float (l,float_of_string s)
           else if S.contains s 'e' || S.contains s 'E'
                  then Float (l,float_of_string s)
                  else Int (l,int_of_string s)

let mk_loc (buf : lexbuf) : pos =
  Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf

let comments : (pos * string) list ref = ref []

let new_comment (p : pos) (c : string) = 
  comments := (p, c) :: !comments

let block_comment_buf = Buffer.create 120

let comment_start_p = ref dummy_pos

}

(* dec_digit+ corresponds to DecimalDigits in the spec. *)
let dec_digit = ['0'-'9']

let signed_int = dec_digit+ | ('+' dec_digit+) | ('-' dec_digit+)

let expt_part = ['e' 'E'] signed_int

let dec_int_lit = '0' | (['1'-'9'] dec_digit*)

let hex = ['0'-'9' 'A'-'f' 'a'-'f']

let hex_lit = ("0x" | "0X") hex+

let dec_lit = 
  (dec_int_lit '.' dec_digit* expt_part?) | 
  ('.' dec_digit+ expt_part?) |
  (dec_int_lit expt_part?)

let num_lit = dec_lit | hex_lit

let ident = ['a'-'z' 'A'-'Z' '$' '_']['a'-'z' 'A'-'Z' '0'-'9' '$' '_']*

let digit = ['0'-'9']

let char = [^ '"' '\\']

let blank = [ ' ' '\t' ]

let escape_sequence
  = [^ '\r' '\n'] | ('x' hex hex) | ('u' hex hex hex hex)

let double_quoted_string_char = 
  [^ '\r' '\n' '"' '\\'] | ('\\' escape_sequence)

let single_quoted_string_char =
  [^ '\r' '\n' '\'' '\\'] | ('\\' escape_sequence)

rule token = parse
   | blank + { token lexbuf }
   | '\n' { new_line lexbuf; token lexbuf }
   | '\r' { new_line lexbuf; token lexbuf }
   | "\r\n" { new_line lexbuf; token lexbuf }
   | "/*" { comment_start_p := lexeme_start_p lexbuf; block_comment lexbuf }
   | "//"([^ '\r' '\n']* as x) [ '\r' '\n' ]
       { comment_start_p := lexeme_start_p lexbuf; 
         new_comment (!comment_start_p, lexeme_end_p lexbuf) x; 
         new_line lexbuf;
         token lexbuf }

   (* ContinueId and BreakId are tokens for labelled break and continue.  They
    * include their target label.
    *)
   | "continue" [ ' ' '\t' ]+ ident as cont 
       { ContinueId (drop_spaces cont 8) }
   | "break" [ ' ' '\t' ]+ ident as cont 
       { BreakId (drop_spaces cont 8) }

   | '/' ([^ '*'] double_quoted_string_char* as x) "/gi"
       { Regexp (x, true, true) }
   | '/' ([^ '*'] double_quoted_string_char* as x) "/g"
       { Regexp (x, true, false) }
   | '/' ([^ '*'] double_quoted_string_char* as x) "/i"
       { Regexp (x, false, true) }
   | '/' ([^ '*'] double_quoted_string_char* as x) "/"
       { Regexp (x, false, false) }

   | '"' (double_quoted_string_char* as x) '"'
     { String (mk_loc lexbuf, x) }
   | ''' (single_quoted_string_char* as x) '''
     { String (mk_loc lexbuf, x) }
   
   | num_lit as x { parse_num_lit x (mk_loc lexbuf) }
   | "{" { LBrace }
   | "}" { RBrace }
   | '(' { LParen }
   | ')' { RParen }
   | "|=" { AssignOp OpAssignBOr }
   | "^=" { AssignOp OpAssignBXor }
   | "&=" { AssignOp OpAssignBAnd }
   | "<<=" { AssignOp OpAssignLShift }
   | ">>=" { AssignOp OpAssignZfRShift }
   | ">>>=" { AssignOp OpAssignSpRShift }
   | "+=" { AssignOp OpAssignAdd }
   | "-=" { AssignOp OpAssignSub }
   | "*=" { AssignOp OpAssignMul }
   | "/=" { AssignOp OpAssignDiv }
   | "%=" { AssignOp OpAssignMod }
   | "%" { Mod }
   | "=" { Assign }
   | ";" { Semi }
   | "," { Comma }
   | "?" { Ques }
   | ":" { Colon }
   | "||" { LOr }
   | "&&" { LAnd }
   | "|" { BOr }
   | "^" { BXor }
   | "&" { BAnd }
   | "===" { StrictEq }
   | "==" { AbstractEq }
   | "!=" { AbstractNEq }
   | "!==" { StrictNEq }
   | "<<" { LShift }
   | ">>" { RShift }
   | ">>>" { SpRShift }
   | "<=" { LEq }
   | "<" { LT }
   | ">=" { GEq }
   | ">" { GT }
   | "++" { PlusPlus }
   | "--" { MinusMinus }
   | "+" { Plus }
   | "-" { Minus }
   | "*" { Times }
   | "/" { Div }
   | "!" { Exclamation }
   | "~" { Tilde }
   | "!" { Exclamation }
   | "." { Period }
   | "[" { LBrack }
   | "]" { RBrack }

   | "if" { If  }
   | "else" { Else  }
   | "true" { True  }
   | "false" { False  }
   | "new" { New  }
   | "instanceof" { Instanceof  }
   | "this" { This  }
   | "null" { Null  }
   | "function" { Function  }
   | "typeof" { Typeof  }
   | "void" { Void  }
   | "delete" { Delete  }
   | "switch" { Switch  }
   | "default" { Default  }
   | "case" { Case  }
   | "while" { While  }
   | "do" { Do  }
   | "break" { Break  }
   | "var" { Var  }
   | "in" { In  }
   | "for" { For  }
   | "try" { Try  }
   | "catch" { Catch  }
   | "finally" { Finally  }
   | "throw" { Throw  }
   | "return" { Return  }
   | "with" { With  }
   | "continue" { Continue  }
   | "instanceof" { Instanceof  }

   | ident as x { Id (mk_loc lexbuf,x) }

  
   | eof { EOF }

and block_comment = parse
    "*/" 
      { new_comment (!comment_start_p, lexeme_end_p lexbuf)
          (Buffer.contents block_comment_buf);
        Buffer.clear block_comment_buf;
        token lexbuf }
  | '*'
      { Buffer.add_char block_comment_buf '*';
        block_comment lexbuf }
  | [ '\n' '\r' ] 
      { new_line lexbuf;
        Buffer.add_char block_comment_buf '\n';
        block_comment lexbuf }
  | ([^ '\n' '\r' '*'])+ as txt
      { Buffer.add_string block_comment_buf txt;
        block_comment lexbuf }

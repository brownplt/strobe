open Typedjs_lexer
open Typedjs_parser
open Typedjs_fromExpr

let from_exprjs exprjs comments env = 
  init_types comments;
  from_exprjs env exprjs

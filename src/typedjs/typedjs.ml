open Typedjs_lexer
open Typedjs_parser
open Typedjs_fromExpr
open Typedjs_anf

let from_exprjs exprjs comments = 
  init_types comments;
  from_exprjs exprjs

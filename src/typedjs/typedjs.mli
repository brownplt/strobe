open Prelude
open JavaScript_syntax
open Typedjs_syntax

val from_exprjs : Exprjs_syntax.expr -> (pos * string) list -> pos exp

open Typedjs_syntax
open Prelude
open JavaScript_syntax

val init_types : (pos * string) list -> unit

val from_exprjs : pos Exprjs_syntax.expr -> pos exp


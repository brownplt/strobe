open Typedjs_syntax
open Prelude
open JavaScript_syntax

val init_types : (pos * string) list -> unit

val from_exprjs : Exprjs_syntax.expr -> def list


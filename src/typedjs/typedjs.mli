open Prelude
open JavaScript_syntax
open Typedjs_syntax
open Typedjs_env

val from_exprjs : Exprjs_syntax.expr -> (pos * string) list -> Env.env -> def

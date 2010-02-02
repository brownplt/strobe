open Prelude
open JavaScript_syntax

val render_expr : pos expr -> string

val render_stmts : pos stmt list -> string

val render_prefixOp : prefixOp -> string

val render_infixOp : infixOp -> string

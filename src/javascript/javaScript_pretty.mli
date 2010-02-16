open Prelude
open JavaScript_syntax

val render_expr : expr -> string

val render_stmts : stmt list -> string

val render_prefixOp : prefixOp -> string

val render_infixOp : infixOp -> string

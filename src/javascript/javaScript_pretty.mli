open Prelude
open FormatExt
open JavaScript_syntax

val p_const : const -> printer
val render_expr : expr -> string

val render_prog : prog -> string

val render_prefixOp : prefixOp -> string

val render_infixOp : infixOp -> string

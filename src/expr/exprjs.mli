open Prelude

val from_javascript : JavaScript_syntax.stmt list -> pos Exprjs_syntax.expr

val print_expr : 'a Exprjs_syntax.expr -> unit

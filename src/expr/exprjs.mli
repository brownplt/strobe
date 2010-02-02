open Prelude

val from_javascript : pos JavaScript_syntax.stmt list -> pos Exprjs_syntax.expr

val print_expr : 'a Exprjs_syntax.expr -> unit

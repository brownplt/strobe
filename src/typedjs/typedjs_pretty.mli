open Prelude

val pretty_exp : Format.formatter -> 'a Typedjs_syntax.exp -> unit

val print_exp : 'a Typedjs_syntax.exp -> unit

val pretty_typ : Format.formatter -> Typedjs_syntax.typ -> unit

val string_of_typ : Typedjs_syntax.typ -> string

open Prelude
open Typedjs_syntax

val tc_const : JavaScript_syntax.const -> typ

val typ_of_value : exp -> typ

exception Not_value of string

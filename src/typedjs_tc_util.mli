open Prelude
open Typedjs_syntax

val tc_const : JavaScript_syntax.const -> TypImpl.typ

val typ_of_value : exp -> TypImpl.typ

exception Not_value of string

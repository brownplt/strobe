open Prelude
open Typedjs_syntax

val tc_const : JavaScript_syntax.const -> typ

val typ_of_value : exp -> typ

val typ_of_value_init : exp -> typ

val cmp_props : string * typ -> string * typ -> int

exception Not_value of string

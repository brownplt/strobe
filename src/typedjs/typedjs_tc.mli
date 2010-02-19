open Prelude
open Typedjs_syntax

exception Typ_error of pos * string

val tc_exp : Env.env -> exp -> typ

val typecheck : def -> unit

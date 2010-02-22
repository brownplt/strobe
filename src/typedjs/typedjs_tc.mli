open Prelude
open Typedjs_syntax

val tc_exp : Env.env -> exp -> typ

val typecheck : Env.env -> def -> unit

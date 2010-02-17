open Prelude
open Typedjs_syntax


val tc_exp : Env.env -> exp -> typ

val tc_defs : Env.env -> def list -> unit

val typecheck : def list -> unit

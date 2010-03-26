open Prelude
open Typedjs_syntax
open Typedjs_env

val tc_exp : Env.env -> exp -> typ

val typecheck : Env.env -> def -> unit

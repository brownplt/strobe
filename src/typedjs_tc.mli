open Prelude
open Typedjs_syntax
open Typedjs_env
open Typedjs_dyn

val contracts : (int * typ) IntMap.t ref

val tc_exp : Env.env -> exp -> typ

val typecheck : Env.env -> def -> unit

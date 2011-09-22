open Prelude
open Typedjs_syntax
open Typedjs_env
open Typedjs_dyn

val contracts : (int * typ) IntMap.t ref

val disable_flows : unit -> unit

val typecheck : env -> exp -> unit

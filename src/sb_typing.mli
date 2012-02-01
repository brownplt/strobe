open Prelude
open Typedjs_syntax
open Typedjs_env

val disable_flows : unit -> unit

val typecheck : env -> exp -> unit

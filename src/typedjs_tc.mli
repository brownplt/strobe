open Prelude
open Typedjs_syntax
open Typedjs_env
open Typedjs_dyn

val contracts : (int * typ) IntMap.t ref

val disable_unreachable_check : unit -> unit

(** Prints a warning when the type of an applied function, [T] is
    [Native]. i.e., when [Native <: T]. *)
val print_native : unit -> unit

val tc_exp : Env.env -> exp -> typ

val typecheck : Env.env -> def -> Env.env

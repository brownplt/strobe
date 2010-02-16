open Prelude
open Format
open Typedjs_syntax

val pretty_exp : formatter -> exp -> unit

val pretty_typ : formatter -> typ -> unit

val pretty_runtime_typ : formatter -> runtime_typ -> unit

val pretty_abs_value : formatter -> abs_value -> unit

val pretty_def : formatter -> def -> unit

val pretty_defs : formatter -> def list -> unit

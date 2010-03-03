open Prelude
open Format
open FormatExt
open Typedjs_syntax

val pretty_exp : formatter -> exp -> unit

val pretty_typ : formatter -> typ -> unit

val pretty_def : formatter -> def -> unit

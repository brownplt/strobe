open Prelude
open Typedjs_syntax

exception Kind_error of string

val kind_check : kind IdMap.t -> typ -> kind

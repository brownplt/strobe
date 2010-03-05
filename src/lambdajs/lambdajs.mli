open Lambdajs_syntax
open Prelude

val parse_lambdajs : in_channel -> string -> exp
val parse_env : in_channel -> string -> (id * exp) list

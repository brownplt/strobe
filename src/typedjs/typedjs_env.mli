open Prelude
open Typedjs_syntax

val parse_env : in_channel -> string -> env_decl list

val mk_env : env_decl list -> Env.env

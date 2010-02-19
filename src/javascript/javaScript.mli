open Prelude
open JavaScript_syntax

val parse_javascript : in_channel -> string ->  prog * (pos * string) list

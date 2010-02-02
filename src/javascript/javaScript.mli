open Prelude
open JavaScript_syntax

val parse_javascript : in_channel  -> string  
  -> pos stmt list * (pos * string) list

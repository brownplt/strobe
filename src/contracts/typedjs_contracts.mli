open Prelude

exception Typ_to_contract of pos * string

val types_to_contracts : JavaScript_syntax.prog -> JavaScript_syntax.prog

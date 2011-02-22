open Prelude

type regex = RegLang_syntax.regex

val parse_regex : Lexing.position -> string -> regex

type fsm

val fsm_of_regex : regex -> fsm
val intersect : fsm -> fsm -> fsm
val negate : fsm -> fsm
val nullable : fsm -> bool 
val is_finite : fsm -> bool
val overlap : fsm -> fsm -> bool
val subtract : fsm -> fsm -> fsm
val contains : fsm -> fsm -> bool
val is_empty : fsm -> bool

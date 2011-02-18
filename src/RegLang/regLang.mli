open Prelude

module CharSet : Set.S
  with type elt = Char.t

type regex =
  | InSet of CharSet.t
  | NotInSet of CharSet.t
  | Alt of regex * regex
  | Star of regex
  | Empty
  | String of string
  | Concat of regex * regex
  | AnyChar

type fsm

val fsm_of_regex : regex -> fsm
val intersect : fsm -> fsm -> fsm
val nullable : fsm -> bool 
val contains : fsm -> fsm -> bool


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

type label =
  | Epsilon
  | In of CharSet.t
  | NotIn of CharSet.t

type state = 
    | I of int
    | II of (state * state)

module State : sig
  type t = state
  val compare : t -> t -> int 
end

module StateMap : Map.S
  with type key = State.t

module StateMapExt : MapExt.S
  with type key = State.t
  and type +'a t = 'a StateMap.t

type fsm = {
  edges: (label * state) list StateMap.t;
  start: state;
  accept: state;
}

val nfa_of_regex : regex -> fsm
val intersect : fsm -> fsm -> fsm
val nullable : fsm -> bool 
val contains : fsm -> fsm -> bool


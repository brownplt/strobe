type t =
  | And of t * t
  | Or of t * t
  | Not of t
  | Var of Id.t
  | True
  | False
  | Imp of t * t
  | Eql of t * t

val is_sat : t -> bool

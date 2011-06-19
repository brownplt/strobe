open Sig

module Make : functor (P : PAT) -> 
  (TYP with type pat = P.t)

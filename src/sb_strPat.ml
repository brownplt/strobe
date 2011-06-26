open Prelude

module R = PatReg
module S = PatSets

type t = 
  | Reg of R.t
  | Set of S.t

let parse pos str = Reg (R.parse pos str)

let singleton str = Set (S.singleton str)

let singleton_string v = match v with
  | Reg r -> R.singleton_string r
  | Set s -> S.singleton_string s

let empty = Set S.empty

let all = Set S.all

let intersect v1 v2 = match v1, v2 with
  | Set s1, Set s2 -> Set (S.intersect s1 s2)
  | Reg r1, Reg r2 -> Reg (R.intersect r1 r2)
  | Set s, Reg r
  | Reg r, Set s -> Reg (R.intersect r (S.to_nfa s))

let union v1 v2 = match v1, v2 with
  | Set s1, Set s2 -> Set (S.union s1 s2)
  | Reg r1, Reg r2 -> Reg (R.union r1 r2)
  | Set s, Reg r
  | Reg r, Set s -> Reg (R.union r (S.to_nfa s))

let subtract v1 v2 = match v1, v2 with
  | Set s1, Set s2 -> Set (S.subtract s1 s2)
  | Reg r1, Reg r2 -> Reg (R.subtract r1 r2)
  | Set s, Reg r
  | Reg r, Set s -> Reg (R.subtract r (S.to_nfa s))

let negate v = match v with
  | Set s -> Set (S.negate s)
  | Reg r -> Reg (R.negate r)

let concat _ _ = failwith "concat NYI"

let is_empty v = match v with
  | Set s -> S.is_empty s
  | Reg r -> R.is_empty r

let is_subset v1 v2 = 
  if v1 == v2 then true
  else match v1, v2 with
    | Set s1, Set s2 -> S.is_subset s1 s2
    | Reg r1, Reg r2 -> R.is_subset r1 r2
    | Set s, Reg r -> R.is_subset (S.to_nfa s) r
    | Reg r, Set s -> R.is_subset r (S.to_nfa s)

let is_overlapped v1 v2 = 
  if v1 == v2 then true
  else match v1, v2 with
    | Set s1, Set s2 -> S.is_overlapped s1 s2
    | Reg r1, Reg r2 -> R.is_overlapped r1 r2
    | Set s, Reg r -> R.is_overlapped (S.to_nfa s) r
    | Reg r, Set s -> R.is_overlapped r (S.to_nfa s)

let is_equal v1 v2 = 
  if v1 == v2 then true
  else match v1, v2 with
    | Set s1, Set s2 -> S.is_equal s1 s2
    | Reg r1, Reg r2 -> R.is_equal r1 r2
    | Set s, Reg r -> R.is_equal (S.to_nfa s) r
    | Reg r, Set s -> R.is_equal r (S.to_nfa s)

let example v = match v with
  | Set s -> S.example s
  | Reg r -> R.example r
 
let pretty v = match v with
  | Set s -> S.pretty s
  | Reg r -> R.pretty r

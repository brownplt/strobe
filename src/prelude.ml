open Lexing

type id = string

(** We track the start and end position of each syntactic form. *)
type pos = Lexing.position * Lexing.position 

module IdOrderedType = struct
  type t = id
  let compare = Pervasives.compare
end

module PosOrderedType = struct

  type t = pos

  let compare = Pervasives.compare
   
end

module IdSet = Set.Make (IdOrderedType)

module IdSetExt = SetExt.Make (IdSet)

module PosSet = Set.Make (PosOrderedType)

module PosSetExt = SetExt.Make (PosSet)

module PosMap = Map.Make (PosOrderedType)

module PosMapExt = MapExt.Make (PosOrderedType)(PosMap)

module IdMap = Map.Make (IdOrderedType)

module IdMapExt = MapExt.Make (IdOrderedType) (IdMap)

let fold_left = List.fold_left

let fold_right = List.fold_right

let map = List.map

let printf = Printf.printf

let eprintf = Printf.eprintf

let sprintf = Printf.sprintf

let second2 f (a, b) = (a, f b)

let string_of_position (p, _) = 
  Format.sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let snd3 (a, b, c) = b

let snd2 (a, b) = b

let fst2 (a, b) = a

let fst3 (a, _, _) = a

let thd3 (_, _, c) = c

let rec intersperse a lst = match lst with
    [] -> []
  | [x] -> [x]
  | x :: xs -> x :: a :: (intersperse a xs)

let rec take_while f xs = match xs with
    [] -> [], []
  | x :: xs' -> 
      if f x then
        let lhs, rhs = take_while f xs' in
          x :: lhs, rhs
      else
        [], xs


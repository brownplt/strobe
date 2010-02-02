open Lexing

type id = string

type pos = Lexing.position

module IdOrderedType = struct
  type t = id
  let compare = Pervasives.compare
end

module PosOrderedType = struct

  type t = Lexing.position

  let compare p1 p2 = match p1.pos_fname = p2.pos_fname with
      true -> (match p1.pos_bol - p2.pos_bol with
                   0 -> p1.pos_cnum - p2.pos_cnum
                 | n -> n)
    | false -> 
        failwith (Format.sprintf 
                    "cannot compare positions from different sources: %s and %s"
                    p1.pos_fname p2.pos_fname)
    
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

let sprintf = Printf.sprintf

let second2 f (a, b) = (a, f b)

let string_of_position p = 
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


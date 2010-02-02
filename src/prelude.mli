type id = string

type pos = Lexing.position * Lexing.position

module IdSet : Set.S 
  with type elt = id

module IdSetExt : SetExt.S 
  with type elt = id 
  and type t = IdSet.t

module PosSet : Set.S 
  with type elt = pos

module PosSetExt : SetExt.S 
  with type elt = pos
  and type t = PosSet.t

module PosMap : Map.S
  with type key = pos

module PosMapExt : MapExt.S
  with type key = pos
  with type +'a t = 'a PosMap.t

module IdMap : Map.S
  with type key = id

module IdMapExt : MapExt.S
  with type key = id
  with type +'a t = 'a IdMap.t


val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

val map : ('a -> 'b) -> 'a list -> 'b list

val second2 : ('b -> 'c) -> 'a * 'b -> 'a * 'c

val string_of_position : pos -> string

val snd3 : 'a * 'b * 'c -> 'b

val snd2 : 'a * 'b -> 'b

val fst2 : 'a * 'b -> 'a

val fst3 : 'a * 'b * 'c -> 'a

val thd3 : 'a * 'b * 'c -> 'c

val printf : ('a, out_channel, unit) format -> 'a

val eprintf : ('a, out_channel, unit) format -> 'a

val sprintf : ('a, unit, string) format -> 'a

val intersperse : 'a -> 'a list -> 'a list

val take_while : ('a -> bool) -> 'a list -> 'a list * 'a list

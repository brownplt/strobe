(** Type of string patterns. *)
type t

(** Parse a string representing a pattern. *)
val parse : Lexing.position -> string -> t

val singleton : string -> t
val singleton_string : t -> string option

val empty : t
val all : t

val intersect : t -> t -> t
val union : t -> t -> t
val negate : t -> t
val subtract : t -> t -> t
val concat : t -> t -> t

val is_empty : t -> bool
val is_finite : t -> bool
val is_overlapped : t -> t -> bool

(** [contains pat1 pat2] is true if all strings in [pat1] are also in [pat2]. *)
val contains : t -> t -> bool

(** [example pat] returns an example of a string in [pat]. *)
val example : t -> string option

val pretty : t -> string

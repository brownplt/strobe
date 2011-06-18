type t

val  gen_id : unit -> t
val id_of_string : string -> t

val string_of_id : t -> string

val compare : t -> t -> int

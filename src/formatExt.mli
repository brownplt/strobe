(** Helper functions for working with the builtin [Format] library. *)
open Format

type printer = formatter -> unit

val nest : printer -> printer

val sep : printer list -> printer

val vert : printer list -> printer

val horz : printer list -> printer

val text : string -> printer

val int : int -> printer

val enclose : string -> string -> printer list -> printer

val parens : printer list -> printer

val braces : printer list -> printer

val brackets : printer list -> printer

val angles : printer list -> printer

(** [to_string f x] uses [Format.str_formatter] as the buffer for printing [x]
    with [f]. *)
val to_string : ('a -> printer) -> 'a -> string


val use_std_formatter : unit -> unit
val use_svg_formatter : unit -> unit

val svg_line : int ref
val svg_col : int ref

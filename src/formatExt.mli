(** Helper functions for working with the builtin [Format] library. *)
open Format

type printer = formatter -> unit

val nest : printer -> printer

val sep : printer list -> printer

val squish : printer list -> printer

val vert : printer list -> printer

val horz : printer list -> printer

val text : string -> printer

val int : int -> printer

val enclose : string -> string -> printer -> printer

val parens : printer -> printer

val braces : printer -> printer

val brackets : printer -> printer

val angles : printer -> printer

val set_html_formatter_tag_functions : unit -> unit

val tag : string -> printer -> printer

(*val mark : string -> printer*)

(** [to_string f x] uses [Format.str_formatter] as the buffer for printing [x]
    with [f]. *)
val to_string : ('a -> printer) -> 'a -> string


val svg_formatter : formatter
val flush_svg_formatter : unit -> string

val xml_escape : string -> string

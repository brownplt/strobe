open Prelude

module CharSet : Set.S
  with type elt = Char.t

module CharSetExt : SetExt.S 
  with type elt = Char.t
  and type t = CharSet.t

type regex =
  | InSet of CharSet.t
  | NotInSet of CharSet.t
  | Alt of regex * regex
  | Star of regex
  | Empty
  | String of string
  | Concat of regex * regex
  | Negate of regex

val any_str : regex

val build_range : char -> char -> CharSet.t

module Pretty : sig

  open Format
  open FormatExt

  val p_re : regex -> printer
  val string_of_re : regex -> string

end


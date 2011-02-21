open Prelude

module CharSet = Set.Make (Char)
module CharSetExt = SetExt.Make (CharSet)

type regex =
  | InSet of CharSet.t
  | NotInSet of CharSet.t
  | Alt of regex * regex
  | Star of regex
  | Empty
  | String of string
  | Concat of regex * regex

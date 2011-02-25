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

let build_range first last =
  let ascii_first, ascii_last = Char.code first, Char.code last in
    if not (ascii_last > ascii_first) then
      failwith (sprintf "Bad character range in regex %s-%s" 
                  (Char.escaped first)
                  (Char.escaped last))
    else
      let rec f i chars = 
        if i > ascii_last then chars
        else f (i + 1) (CharSet.add (Char.chr i) chars) in
        f ascii_first CharSet.empty

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
  | Negate of regex

let compare re1 re2 = match (re1, re2) with
  | NotInSet s1, NotInSet s2
  | InSet s1, InSet s2 -> 
    CharSet.compare s1 s2
  | _ ->
    Pervasives.compare re1 re2

let any_str = Star (NotInSet CharSet.empty)

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

module Pretty = struct

  open Format
  open FormatExt
    
  let list_to_ranges (chs : char list) =
    let rec split range ix chlist = match chlist with
      | [] -> [range]
      | ch::chs -> match range with
            (first, last) -> if (ix + 1) = (Char.code ch) 
            then split (first, ch) (ix + 1) chs
            else range::(split (ch, ch) (Char.code ch) chs) in
      match chs with
        | [] -> []
        | ch::chs -> split (ch, ch) (Char.code ch) chs

  let range pair = match pair with 
    | (c1, c2) when c1 = c2 -> text (Char.escaped c1)
    | (c1, c2) -> 
        squish [text (Char.escaped c1); text "-"; 
                text (Char.escaped c2)]

  let rec p_re re = match re with
    | InSet chs -> let ranges = list_to_ranges (CharSet.elements chs) in
        brackets (horz (map range ranges))
    | NotInSet chs -> let ranges = list_to_ranges (CharSet.elements chs) in
        brackets (horz ((text "^")::(map range ranges)))
    | Alt (re1, re2) -> parens (horz [p_re re1; text "|"; p_re re2])
    | Star re -> parens (squish [p_re re; text "*"])
    | Empty -> text "empty"
    | String s -> text s
    | Concat (re1, re2) -> squish [p_re re1; p_re re2]
    | Negate re -> squish [text "complement"; parens (p_re re)]

  let string_of_re = FormatExt.to_string p_re

end

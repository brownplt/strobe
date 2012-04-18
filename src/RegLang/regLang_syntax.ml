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
  | Subtract of regex * regex
  | Inter of regex * regex

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

  let priority s = match s with
    | String _ -> 0
    | Star _ -> 1
    | Concat _ -> 2
    | Negate _ -> 3
    | Subtract _ -> 4
    | Inter _ -> 5
    | Alt _ -> 6 
    | _ -> 7
  let rec parensIf r s =
    let pr = priority r in
    let ps = priority s in
    if (pr < ps || pr = 4 && ps = 4) then parens (p_re s) else p_re s 
  and p_re re = match re with
    | InSet chs -> let ranges = list_to_ranges (CharSet.elements chs) in
        brackets (horz (map range ranges))
    | NotInSet chs -> let ranges = list_to_ranges (CharSet.elements chs) in
        brackets (horz ((text "^")::(map range ranges)))
    | Alt (re1, re2) -> squish [parensIf re re1; text "|"; parensIf re re2]
    | Star re -> squish [parensIf re re; text "*"]
    | Empty -> text "empty"
    | String s -> text s
    | Inter(r1, r2) -> squish [parensIf re r1; text "&"; parensIf re r2]
    | Concat (re1, re2) -> squish [parensIf re re1; parensIf re re2]
    | Negate re -> squish [text "^"; parens (parensIf re re)]
    | Subtract(r1, r2) -> squish [parensIf re r1; text "\\"; parensIf re r2]

  let string_of_re = FormatExt.to_string p_re

end

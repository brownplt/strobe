open Prelude
open RegLang

(** Type of string patterns. *)
type t = 
  | Dual of RegLang.regex * RegLang.fsm Lazy.t
  | Machine of RegLang.fsm
  | StrPlus of IdSet.t
  | StrMinus of IdSet.t

(** Parse a string representing a pattern. *)
let parse pos str =
  let re = RegLang.parse_regex pos str in
  Dual (re, lazy (RegLang.fsm_of_regex re))

let singleton str = StrPlus (IdSet.add str IdSet.empty)

let singleton_string pat = match pat with
  | StrPlus strs -> if (IdSet.cardinal strs) = 1 
    then Some (IdSet.choose strs) else None
  |_ -> None

let mach pat = match pat with
  | Dual (_, m) -> Lazy.force m
  | Machine m -> m
  | StrPlus strs -> 
    RegLang.fsm_of_regex
      (IdSet.fold (fun str re -> 
        if re = RegLang_syntax.Empty then RegLang_syntax.String str else
        RegLang_syntax.Alt (RegLang_syntax.String str, re))
         strs RegLang_syntax.Empty)
  | StrMinus strs ->
    RegLang.negate
      (RegLang.fsm_of_regex
         (IdSet.fold (fun str re -> 
           if re = RegLang_syntax.Empty then RegLang_syntax.String str else
           RegLang_syntax.Alt (RegLang_syntax.String str, re))
            strs RegLang_syntax.Empty))

let empty = StrPlus IdSet.empty

let all = StrMinus IdSet.empty

let intersect pat1 pat2 = match (pat1, pat2) with
  | StrPlus strs1, StrPlus strs2 -> StrPlus (IdSet.inter strs1 strs2)
  | StrMinus strs2, StrPlus strs1
  | StrPlus strs1, StrMinus strs2 -> StrPlus (IdSet.diff strs1 strs2)
  | StrMinus strs1, StrMinus strs2 -> StrMinus (IdSet.union strs1 strs2)
  | _ ->
    Machine (RegLang.intersect (mach pat1) (mach pat2))

let union pat1 pat2 = match (pat1, pat2) with
  | Dual (re1, _), Dual (re2, _) ->
    let re = RegLang_syntax.Alt (re1, re2) in
    Dual (re, lazy (RegLang.fsm_of_regex re))
  | StrPlus strs1, StrPlus strs2 -> StrPlus (IdSet.union strs1 strs2)
  | StrMinus strs2, StrPlus strs1
  | StrPlus strs1, StrMinus strs2 -> StrMinus (IdSet.diff strs2 strs1)
  | StrMinus strs1, StrMinus strs2 -> StrMinus (IdSet.inter strs1 strs2)
  | _ -> Machine (RegLang.union (mach pat1) (mach pat2))

let negate pat = match pat with
  | StrPlus strs1 -> StrMinus strs1
  | StrMinus strs1 -> StrPlus strs1
  | _ -> Machine (RegLang.negate (mach pat))

let subtract pat1 pat2 = match pat1, pat2 with
  | StrPlus strs1, StrPlus strs2 -> StrPlus (IdSet.diff strs1 strs2)
  | StrPlus strs1, StrMinus strs2 -> StrPlus (IdSet.inter strs1 strs2)
  | StrMinus strs1, StrPlus strs2 -> StrMinus (IdSet.union strs1 strs2)
  | StrMinus strs1, StrMinus strs2 -> StrPlus (IdSet.diff strs2 strs1)
  | _ -> Machine (RegLang.subtract (mach pat1) (mach pat2))

let concat pat1 pat2 = match (pat1, pat2) with
  | Dual (re1, _), Dual (re2, _) -> 
    let re' = RegLang_syntax.Concat (re1, re2) in
    Dual (re', lazy (RegLang.fsm_of_regex re'))
  | _ -> all

let is_empty pat = match pat with
  | StrPlus strs -> (IdSet.cardinal strs) = 0
  | StrMinus strs -> false
  | _ -> RegLang.is_empty (mach pat)

let is_finite pat = match pat with
  | StrPlus strs -> true
  | StrMinus strs -> false
  | _ -> RegLang.is_finite (mach pat)

let is_overlapped pat1 pat2 = not (is_empty (intersect pat1 pat2))

let contains pat1 pat2 = 
  is_empty (intersect pat1 (negate pat2))

let is_member str pat = match pat with
  | StrPlus strs -> IdSet.mem str strs
  | StrMinus strs -> not (IdSet.mem str strs)
  | _ -> 
    RegLang.contains
      (RegLang.fsm_of_regex (RegLang_syntax.String str))
      (mach pat)

let is_equal pat1 pat2 = match (pat1, pat2) with
  | StrPlus strs1, StrPlus strs2 -> IdSet.equal strs1 strs2
  | Dual (re1, mach1), Dual (re2, mach2) ->
    RegLang_syntax.compare re1 re2 = 0 (* falsely says not equal negative *)
  | _ ->
    contains pat1 pat2 && contains pat2 pat1

let example pat = match pat with
  | StrPlus strs -> if (IdSet.cardinal strs) >= 1 
    then Some (IdSet.choose strs) else None
  | _ -> RegLang.find_word (mach pat)

let pretty pat = 
  let open FormatExt in
      match pat with
        | Dual (re, _) -> RegLang_syntax.Pretty.string_of_re re
        | StrPlus strs -> FormatExt.to_string (IdSetExt.p_set text) strs
        | StrMinus strs -> "NOT(" ^ FormatExt.to_string (IdSetExt.p_set text) strs ^ ")"
        | Machine _ -> "** unprintable DFA **"

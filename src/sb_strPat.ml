open Prelude
open RegLang

(** Type of string patterns. *)
type t = 
  | Dual of RegLang.regex * RegLang.fsm Lazy.t
  | Machine of RegLang.fsm
  | Singleton of string

(** Parse a string representing a pattern. *)
let parse pos str =
  let re = RegLang.parse_regex pos str in
  Dual (re, lazy (RegLang.fsm_of_regex re))

let singleton str = Singleton str

let singleton_string pat = match pat with
  | Singleton str -> Some str
  |_ -> None

let mach pat = match pat with
  | Dual (_, m) -> Lazy.force m
  | Machine m -> m
  | Singleton str -> RegLang.fsm_of_regex (RegLang.parse_regex Lexing.dummy_pos str)

let empty = 
  let open RegLang_syntax in
  let re = InSet CharSet.empty in
  let fsm = RegLang.fsm_of_regex re in
  Dual (re, lazy fsm)

let all = 
  Dual (RegLang_syntax.any_str,
        lazy (RegLang.fsm_of_regex RegLang_syntax.any_str))


let intersect pat1 pat2 = match (pat1, pat2) with
  | Singleton str1, Singleton str2 ->
    if str1 = str2 then pat1
    else empty
  | _ ->
    Machine (RegLang.intersect (mach pat1) (mach pat2))

let union pat1 pat2 = match (pat1, pat2) with
  | Singleton str1, Singleton str2 when str1 = str2 -> pat1
  | Singleton s, Dual (re, _)
  | Dual (re, _), Singleton s ->
    let re' = RegLang_syntax.Alt (RegLang_syntax.String s, re) in
    Dual (re', lazy (RegLang.fsm_of_regex re'))
  | Dual (re1, _), Dual (re2, _) ->
    let re = RegLang_syntax.Alt (re1, re2) in
    Dual (re, lazy (RegLang.fsm_of_regex re))
  | _ -> Machine (RegLang.union (mach pat1) (mach pat2))

let negate pat = Machine (RegLang.negate (mach pat))

let subtract pat1 pat2 = Machine (RegLang.subtract (mach pat1) (mach pat2))

let concat pat1 pat2 = match (pat1, pat2) with
  | Singleton str1, Singleton str2 -> Singleton (str1 ^ str2)
  | Dual (re1, _), Dual (re2, _) -> 
    let re' = RegLang_syntax.Concat (re1, re2) in
    Dual (re', lazy (RegLang.fsm_of_regex re'))
  | _ -> all

let is_empty pat = RegLang.is_empty (mach pat)

let is_finite pat = match pat with
  | Singleton _ -> true
  | _ -> RegLang.is_finite (mach pat)

let is_overlapped pat1 pat2 = match pat1, pat2 with
  | Singleton str1, Singleton str2 -> str1 = str2
  | _ ->
    not (RegLang.is_empty (RegLang.intersect (mach pat1) (mach pat2)))

let contains pat1 pat2 = RegLang.contains (mach pat1) (mach pat2)

let is_equal pat1 pat2 = match (pat1, pat2) with
  | Singleton str1, Singleton str2 -> str1 = str2
  | Dual (re1, mach1), Dual (re2, mach2) ->
    RegLang_syntax.compare re1 re2 = 0 (* falsely says not equal negative *)
  | _ ->
    contains pat1 pat2 && contains pat2 pat1

let example pat = match pat with
  | Singleton s -> Some s
  | _ -> RegLang.find_word (mach pat)

let pretty pat = match pat with
  | Singleton str -> str
  | Dual (re, _) -> RegLang_syntax.Pretty.string_of_re re
  | Machine _ -> "** unprintable DFA **"

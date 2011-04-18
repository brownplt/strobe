open Prelude
open RegLang

(** Type of string patterns. *)
type t = 
  | Dual of RegLang.regex * RegLang.fsm Lazy.t
  | Machine of RegLang.fsm

(** Parse a string representing a pattern. *)
let parse pos str =
  let re = RegLang.parse_regex pos str in
  Dual (re, lazy (RegLang.fsm_of_regex re))

let singleton str = 
  let re = RegLang_syntax.String str in
  Dual (re, lazy (RegLang.fsm_of_regex re))

let singleton_string pat = match pat with
  | Dual (RegLang_syntax.String s, _) -> Some s
  | _ -> None

let mach pat = match pat with
  | Dual (_, m) -> Lazy.force m
  | Machine m -> m

let empty = 
  let open RegLang_syntax in
  let re = InSet CharSet.empty in
  let fsm = RegLang.fsm_of_regex re in
  Dual (re, lazy fsm)

let all = 
  Dual (RegLang_syntax.any_str,
        lazy (RegLang.fsm_of_regex RegLang_syntax.any_str))


let intersect pat1 pat2 = Machine (RegLang.intersect (mach pat1) (mach pat2))

let union pat1 pat2 = match (pat1, pat2) with
  | Dual (re1, _), Dual (re2, _) ->
    let re = RegLang_syntax.Alt (re1, re2) in
    Dual (re, lazy (RegLang.fsm_of_regex re))
  | _ -> Machine (RegLang.union (mach pat1) (mach pat2))

let negate pat = Machine (RegLang.negate (mach pat))

let subtract pat1 pat2 = Machine (RegLang.subtract (mach pat1) (mach pat2))

let concat pat1 pat2 = match (pat1, pat2) with
  | Dual (re1, _), Dual (re2, _) -> 
    let re' = RegLang_syntax.Concat (re1, re2) in
    Dual (re', lazy (RegLang.fsm_of_regex re'))
  | _ -> all

let is_empty pat = RegLang.is_empty (mach pat)

let is_finite pat = RegLang.is_finite (mach pat)

let is_overlapped pat1 pat2 =
  not (RegLang.is_empty (RegLang.intersect (mach pat1) (mach pat2)))

let contains pat1 pat2 = RegLang.contains (mach pat1) (mach pat2)

let example pat = RegLang.find_word (mach pat)

let pretty pat = match pat with
  | Dual (re, _) -> RegLang_syntax.Pretty.string_of_re re
  | Machine _ -> "** unprintable DFA **"

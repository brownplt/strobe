open Prelude
open RegLang

open Dprle_nfa
open Sb_regex

module Nfa = Dprle_nfa

(** Type of string patterns. *)
type t = nfa

(** Parse a string representing a pattern. *)
let parse pos str =
  to_nfa (RegLang.parse_regex pos str)

let singleton str = to_nfa (RegLang_syntax.String str)

let singleton_string = gen_string

let empty = new_nfa_states 0 1

let all = new_sigmastar ()

let intersect p1 p2 = 
  simple_intersect p1 p2

let union  p1 p2 = 
  Nfa.union p1 p2

let negate pat  = 
  let pat' = Nfa.nfa_to_dfa pat in
  complement pat';
  pat'

let subtract pat1 pat2 = 
  Nfa.simple_intersect pat1 (negate pat2)

let concat = Nfa.simple_concat

let is_empty = Nfa.is_empty

let is_overlapped pat1 pat2 = 
  printf "Overlapped?\n%!";
  not (is_empty (intersect pat1 pat2))

let is_subset p1 p2 =
  printf "contains\n%!";
  Nfa.nfa_subseteq p1 p2

let is_member str pat = 
  Nfa.nfa_subseteq (to_nfa (RegLang_syntax.String str)) pat

let is_equal p1 p2 = 
  Nfa.nfa_eq p1 p2

let example pat = gen_string pat

let pretty pat =  "** unprintable DFA **"

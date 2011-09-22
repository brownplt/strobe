open Prelude

open Dprle_nfa
open Sb_regex

module Nfa = Dprle_nfa

(** Type of string patterns. *)
type t = nfa * string

(** Parse a string representing a pattern. *)
let parse pos str =
  let lexbuf = Lexing.from_string str in
  try 
    lexbuf.Lexing.lex_curr_p <- pos;
    (to_nfa (RegLang_parser.regex RegLang_lexer.token lexbuf), str)
  with
    |  Failure "lexing: empty token" ->
      failwith (sprintf "error lexing regex %s at %s"
                  str
                  (string_of_position 
                     (lexbuf.Lexing.lex_curr_p, lexbuf.Lexing.lex_curr_p)))
    | RegLang_parser.Error ->
      failwith (sprintf "error parsing regex %s at %s"
                  str
                  (string_of_position 
                     (lexbuf.Lexing.lex_curr_p, lexbuf.Lexing.lex_curr_p)))

let singleton str = (to_nfa (RegLang_syntax.String str), str)

let singleton_string (p, _) = gen_string p

let empty = (new_nfa_states 0 1, "//")

let all = (new_sigmastar (), "/.*/")

let intersect (p1,s1) (p2,s2) = 
  (simple_intersect p1 p2, sprintf "(inter %s %s)" s1 s2)

let union (p1,s1) (p2,s2) = 
  (Nfa.union p1 p2, sprintf "(union %s %s)" s1 s2)

let negate (pat, s) = 
  let pat' = Nfa.nfa_to_dfa pat in
  complement pat';
  (pat', sprintf "(negate %s)" s)

let subtract (pat1, s1) (pat2, s2) = 
  let (pat2', _) = negate (pat2, s2) in
  (Nfa.simple_intersect pat1 pat2', sprintf "(subtract %s %s)" s1 s2)

let concat (p1, s1) (p2, s2) = 
  (Nfa.simple_concat p1 p2, sprintf "(%s)(%s)" s1 s2)

let is_empty (p, _) = Nfa.is_empty p

let is_overlapped t1 t2 =
  not (is_empty (intersect t1 t2))

let is_subset (p1, s1) (p2, s2) = Nfa.nfa_subseteq p1 p2

let is_equal (p1, _) (p2, _) = 
  Nfa.nfa_eq p1 p2

let example = singleton_string

let pretty (_, str) = str

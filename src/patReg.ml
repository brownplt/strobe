open Prelude

open Dprle_nfa
open Sb_regex

module Nfa = Dprle_nfa

(** Type of string patterns. *)
type t = nfa

(** Parse a string representing a pattern. *)
let parse pos str =
  let lexbuf = Lexing.from_string str in
  try 
    lexbuf.Lexing.lex_curr_p <- pos;
    to_nfa (RegLang_parser.regex RegLang_lexer.token lexbuf)
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
  not (is_empty (intersect pat1 pat2))

let is_subset p1 p2 =
  Nfa.nfa_subseteq p1 p2

let is_equal p1 p2 = 
  Nfa.nfa_eq p1 p2

let example pat = gen_string pat

let pretty pat = match gen_string pat with
  | Some s -> "** big DFA including " ^ s ^ " **"
  | None ->  "** unprintable DFA **"

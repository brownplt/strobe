open Prelude

open Dprle_nfa
open Sb_regex

module Nfa = Dprle_nfa

(** Type of string patterns. *)
type regexRepl = 
  | String of string
  | Union of regexRepl * regexRepl
  | Inter of regexRepl * regexRepl
  | Concat of regexRepl * regexRepl
  | Star of regexRepl
  | Negate of regexRepl
  | Subtract of regexRepl * regexRepl

type t = nfa * regexRepl

(** Parse a string representing a pattern. *)
let parse pos str =
  let lexbuf = Lexing.from_string str in
  try 
    lexbuf.Lexing.lex_curr_p <- pos;
    (to_nfa (RegLang_parser.regex RegLang_lexer.token lexbuf), String str)
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

let singleton str = (to_nfa (RegLang_syntax.String str), String str)

let singleton_string (p, _) = gen_string p

let empty = (new_nfa_states 0 1, String "()")

let all = (new_sigmastar (), Star (String "."))

let intersect (p1,s1) (p2,s2) = 
  if (Nfa.is_empty p1) then empty
  else if (Nfa.is_empty p2) then empty
  else (simple_intersect p1 p2, Inter (s1, s2))

let intersections ts = match (List.fold_left 
                         (fun u t -> match u with
                         | None -> Some t
                         | Some u -> Some (intersect u t))
                         None
                         ts) with
  | Some u -> u
  | None -> empty

let union (p1,s1) (p2,s2) = 
  if (Nfa.is_empty p1) then (p2, s2)
  else if (Nfa.is_empty p2) then (p1, s1)
  else (Nfa.union p1 p2, Union (s1, s2))

let unions ts = match (List.fold_left 
                         (fun u t -> match u with
                         | None -> Some t
                         | Some u -> Some (union u t))
                         None
                         ts) with
  | Some u -> u
  | None -> empty

let negate (pat, s) = 
  let pat' = Nfa.nfa_to_dfa pat in
  complement pat';
  (pat', Negate s)


let concat (p1, s1) (p2, s2) = 
  (Nfa.simple_concat p1 p2, Concat (s1, s2))

let is_empty (p, _) = Nfa.is_empty p

let is_overlapped t1 t2 =
  not (is_empty (intersect t1 t2))

let is_subset (p1, s1) (p2, s2) = Nfa.nfa_subseteq p1 p2

let is_equal (p1, _) (p2, _) = 
  Nfa.nfa_eq p1 p2

let example = singleton_string

let pretty (_, str) = 
  let open FormatExt in
  let priority s = match s with
    | String _ -> 0
    | Star _ -> 1
    | Concat _ -> 2
    | Negate _ -> 3
    | Subtract _ -> 4
    | Inter _ -> 5
    | Union _ -> 6 in
  let rec parensIf r s =
    let pr = priority r in
    let ps = priority s in
    if (pr < ps || pr = 4 && ps = 4) then parens (helper s) else helper s 
  and helper r = match r with
    | String s -> text s
    | Star s -> squish[parensIf r s; text "*"]
    | Concat(s1, s2) -> squish [parensIf r s1; parensIf r s2]
    | Inter(s1, s2) -> squish [parensIf r s1; text "&"; parensIf r s2]
    | Union(s1, s2) -> squish [parensIf r s1; text "|"; parensIf r s2]
    | Negate s -> squish [text "!"; parensIf r s]
    | Subtract(s1, s2) -> squish [parensIf r s1; text "\\"; parensIf r s2] in
  squish [text "/"; helper str; text "/"] Format.str_formatter;
  Format.flush_str_formatter()

let subtract (pat1, s1) (pat2, s2) = 
  let (pat2', _) = negate (pat2, s2) in
  let isect = Nfa.simple_intersect pat1 pat2' in
  if Nfa.nfa_eq pat1 isect then (pat1, s1)
  else (isect, Subtract (s1, s2))


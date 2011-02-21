open Prelude

open RegLang_syntax

module H = Hashtbl

type regex = RegLang_syntax.regex

let parse_regex pos str =
  let lexbuf = Lexing.from_string str in
    try 
      lexbuf.Lexing.lex_curr_p <- pos;
      RegLang_parser.regex RegLang_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "lexical error at %s"
                       (string_of_position 
                          (lexbuf.Lexing.lex_curr_p, lexbuf.Lexing.lex_curr_p)))
      | RegLang_parser.Error ->
           failwith (sprintf "parse error at %s"
                       (string_of_position 
                          (lexbuf.Lexing.lex_curr_p, lexbuf.Lexing.lex_curr_p)))


type follow_tbl  = IntSet.t array

type pos = int
      
type aux = {
  nullable : bool;
  first_pos : IntSet.t;
  last_pos : IntSet.t;
}

(* The NFA for [re] requires [num_terminals re] states. *)
let rec num_terminals (re : regex) : int = match re with
  | InSet _
  | NotInSet _ 
  | Empty -> 0
  | String s -> String.length s
  | Star re' -> num_terminals re'
  | Alt (re1, re2)
  | Concat (re1, re2) -> num_terminals re1 + num_terminals re2

(* augmented regular expressions *)

module AugChar = struct
  type t = Char of char | Accept

  let compare = Pervasives.compare

  open FormatExt

  let pp t = match t with
    | Char ch -> fun fmt -> Format.pp_print_char fmt ch
    | Accept -> text "#"

end

module AugCharSet = Set.Make (AugChar)
module AugCharSetExt = SetExt.Make (AugCharSet)
module AugCharMap = Map.Make (AugChar)
module AugCharMapExt = MapExt.Make (AugChar) (AugCharMap)

let make_augCharSet (set : CharSet.t) : AugCharSet.t =
  CharSet.fold (fun ch set' -> AugCharSet.add (AugChar.Char ch) set')
    set AugCharSet.empty

type st = 
  | S of int
  | J of st * st (* used for intersection *)

module St = struct
  type t = st
  let compare = Pervasives.compare
    
  open FormatExt
  
  let rec pp t = match t with
    | S n -> int n
    | J (s1, s2) -> angles (squish [ pp s1; text ","; pp s2 ])
      
end 

module StMap = Map.Make (St)
module StMapExt = MapExt.Make (St) (StMap)
  
module StSet = Set.Make (St)
module StSetExt = SetExt.Make (StSet)
  
      
type leaf = 
  | LeafIn of AugCharSet.t 
  | LeafNotIn of AugCharSet.t

module Leaf = struct

  type t = leaf
      
  module S = AugCharSet

  let one s = LeafIn (AugCharSet.singleton s)

  let is_accepting l = match l with
    | LeafIn s -> S.mem AugChar.Accept s
    | LeafNotIn s -> not (S.mem AugChar.Accept s)
      
  let is_not_empty l = match l with
    | LeafIn s -> not (S.is_empty s)
    | _ -> true
    
  let subset (t1 : t) (t2 : t) : bool = match t1, t2 with
    | LeafIn s1, LeafIn s2 -> AugCharSet.subset s1 s2
    | LeafNotIn s1, LeafNotIn s2 -> AugCharSet.subset s2 s1
    | LeafIn s1, LeafNotIn s2 -> AugCharSet.is_empty (AugCharSet.inter s1 s2)
    | LeafNotIn s1, LeafIn s2 -> AugCharSet.is_empty (AugCharSet.inter s1 s2)
    
  let explode (leaves : leaf list) =
    let rec f incl_chs excl_chs leaves = match leaves with
      | [] -> (incl_chs, excl_chs)
      | LeafIn chs :: leaves' ->
        f (S.union chs incl_chs) 
          (match excl_chs with
            | None -> None
            | Some chs' -> Some (S.union chs' chs))
          leaves'
      | LeafNotIn chs :: leaves' ->
        f incl_chs
          (match excl_chs with
            | None -> Some (S.union incl_chs chs)
            | Some chs' -> Some (S.inter chs chs'))
          leaves' in
    f S.empty None leaves

  open FormatExt

  let pp (t : t) = match t with
    | LeafIn s -> AugCharSetExt.p_set AugChar.pp s
    | LeafNotIn s ->
      horz [ text "complement"; AugCharSetExt.p_set AugChar.pp s ]

end

type edge_list = {
  on_char : st AugCharMap.t;
  other_chars : st
}

type dfa = {
  edges: edge_list  StMap.t;
  start: st;
  accept: StSet.t
}

module DFA = struct
  type t = dfa
    
  open FormatExt

  let p_edge e =
    vert [ AugCharMapExt.p_map AugChar.pp St.pp e.on_char;
           horz [ text "otherwise ->"; St.pp e.other_chars ] ]
  
  let pp t =
    vert
      [ horz [ text "Start: "; St.pp t.start ];
        horz [ text "Accept: "; StSetExt.p_set St.pp t.accept ];
        StMapExt.p_map St.pp p_edge t.edges ]
end

let tables_of_regex (re : regex) =
  let pos = 
    let next_pos = ref 0 in
    fun () -> incr next_pos; !next_pos - 1 in
    (* 1st terminal is at position 0, so +1 for ending # *)
  let max_pos = num_terminals re in 
  let follow_tbl = Array.make (max_pos + 1) IntSet.empty in
  let sym_tbl = Array.make (max_pos + 1) (LeafNotIn AugCharSet.empty) in
  let rec f (re : regex) : aux = match re with
    | Empty -> 
      { nullable = true; first_pos = IntSet.empty; last_pos = IntSet.empty }
    | InSet set -> 
      let p = pos () in
      let s = IntSet.singleton p in
      let ch_set = make_augCharSet set in
      sym_tbl.(p) <- LeafIn ch_set;
      { nullable = false; first_pos = s; last_pos = s }
    | NotInSet set -> 
      let p = pos () in
      let s = IntSet.singleton p in
      let ch_set = make_augCharSet set in
      sym_tbl.(p) <- LeafNotIn ch_set;
      { nullable = false; first_pos = s; last_pos = s }
    | Alt (re1, re2) ->
      let aux1 = f re1 in
      let aux2 = f re1 in
      { nullable = aux1.nullable || aux2.nullable;
        first_pos = IntSet.union aux1.first_pos aux2.first_pos;
        last_pos = IntSet.union aux1.last_pos aux2.last_pos }
    | Concat (re1, re2) ->
      let aux1 = f re1 in
      let aux2 = f re2 in
      IntSet.iter
        (fun last ->
          follow_tbl.(last) <- IntSet.union aux2.first_pos follow_tbl.(last))
        aux1.last_pos;
      { nullable = aux1.nullable && aux2.nullable;
        first_pos = 
          if aux1.nullable then
            IntSet.union aux1.first_pos aux2.first_pos
          else 
            aux1.first_pos;
        last_pos = 
          if aux2.nullable then
            IntSet.union aux1.last_pos aux2.last_pos
          else 
            aux2.last_pos }
    | Star re' ->
      let aux' = f re' in
      IntSet.iter
        (fun last ->
          follow_tbl.(last) <- IntSet.union aux'.first_pos follow_tbl.(last))
        aux'.last_pos;
      { nullable = true;
        first_pos = aux'.first_pos;
        last_pos = aux'.last_pos }
    (* The remaining variants of regex are inessential *)
    | String str ->
      let re = ref Empty in
      String.iter
        (fun ch -> re := Concat (!re, InSet (CharSet.singleton ch)))
        str;
      f !re in
  let aux = f re in
  (* ACat, knowing that accept_aux is not nullable *)
  IntSet.iter
    (fun last ->
      follow_tbl.(last) <- IntSet.add max_pos follow_tbl.(last))
    aux.last_pos;
  sym_tbl.(max_pos) <- LeafIn (AugCharSet.singleton AugChar.Accept);
  let first_state = 
    if aux.nullable then
      IntSet.add max_pos aux.first_pos
    else
      aux.first_pos in
  for i = 0 to max_pos  do
    printf "position %d (%s) followed by%s\n" i
      (FormatExt.to_string Leaf.pp (sym_tbl.(i)))
      (FormatExt.to_string (IntSetExt.p_set FormatExt.int) follow_tbl.(i));
  done;
  (first_state, follow_tbl, sym_tbl)

 (* Page 141, Figure 3.44 *)
let make_dfa (first_state : IntSet.t) (follow : follow_tbl) sym  =
  let next_st_num = ref 0 in
  let next_st () = incr next_st_num; S (!next_st_num - 1) in 
  let accept = ref StSet.empty in
  let edges : edge_list StMap.t ref  = ref StMap.empty in
  let state_names : (IntSet.t * st) list ref = ref [] in
  let err_st = next_st () in
  let rec loop (unmarked_states : (IntSet.t * st) list) = 
    match unmarked_states with
    | [] -> ()
    | (state, st) :: unmarked_states' ->
      (* recurring on the tail of unmarked_states implicitly marks state *)
      (* deviates from the figure *)
      let leaves = map (fun i -> sym.(i)) (IntSet.elements state) in
      (* If any characters are explicitly excluded, those characters
         go to err_st. If no characters are explicitly excluded, the
         wildcard-edge goes to err_st. *)
      let (incl_chs, opt_excl_chs) = Leaf.explode leaves in
      if (AugCharSet.mem AugChar.Accept incl_chs) then
        accept := StSet.add st !accept;
      let more_unmarked_states = ref [] in
      let on_char_map =
        AugCharSet.fold
          (fun ch on_char_map ->
            let next_state =
              IntSet.fold
                (fun i s -> IntSet.union follow.(i) s)
                (IntSet.filter 
                   (fun i -> Leaf.subset (Leaf.one ch) sym.(i)) state)
                IntSet.empty in
            let next_st =
              try snd2 (List.find (fun (n, k) -> IntSet.equal n next_state)
                          !state_names)
              with Not_found ->
                let abbrev = next_st () in
                state_names := (next_state, abbrev) :: !state_names;
                more_unmarked_states := 
                  (next_state, abbrev) :: !more_unmarked_states;
                abbrev in
            AugCharMap.add ch next_st on_char_map)
          incl_chs AugCharMap.empty in
      let edges_from_st = 
        match opt_excl_chs with
          | None ->
            { on_char = on_char_map; other_chars = err_st }
          | Some excl_chs ->
            { on_char =
                AugCharSet.fold (fun ch m -> AugCharMap.add ch err_st m)
                  (AugCharSet.diff excl_chs incl_chs) on_char_map;
              other_chars =
                let next_state = 
                  IntSet.fold
                    (fun i s -> IntSet.union follow.(i) s)
                    (IntSet.filter 
                       (fun i -> Leaf.subset (LeafNotIn excl_chs) sym.(i))
                       state)
                    IntSet.empty in
                try snd2 (List.find (fun (n, k) -> IntSet.equal n next_state)
                            !state_names)
                with Not_found ->
                  let abbrev = next_st () in
                  state_names := (next_state, abbrev) :: !state_names;
                  more_unmarked_states := 
                    (next_state, abbrev) :: !more_unmarked_states;
                  abbrev } in
      edges := StMap.add st (edges_from_st) !edges;
      loop (!more_unmarked_states @ unmarked_states') in
    (* start of algorithm *)
  let first_st = next_st () in
  printf "first state is %s\n" (FormatExt.to_string (IntSetExt.p_set
                                                       FormatExt.int) first_state);
  state_names := [ (first_state, first_st) ];
  loop [ (first_state, first_st) ];
  printf "DFA construction complete!\n";
  { edges = StMap.add err_st
      { on_char = AugCharMap.empty; other_chars = err_st } 
      !edges;
    start = first_st;
    accept = !accept }
    
let cross_edge_list (e1 : edge_list) (e2 : edge_list) =
  { on_char =
      AugCharMap.merge
        (fun ch opt_st1 opt_st2 -> match (opt_st1, opt_st2) with
          | Some st1, Some st2 -> Some (J (st1, st2))
          | Some st1, None -> Some (J (st1, e2.other_chars))
          | None, Some st2 -> Some (J (e1.other_chars, st2))
          | None, None -> failwith "cross_edge_list catastrophe")
        e1.on_char e2.on_char;
    other_chars = J (e1.other_chars, e2.other_chars) }

let intersect dfa1 dfa2 =
  let f state1 (edges1 : edge_list) edgemap1 =
    let g state2 (edges2 : edge_list) edgemap2 =
      StMap.add (J (state1, state2)) (cross_edge_list edges1 edges2) edgemap2 in
    StMap.fold g dfa2.edges edgemap1 in
  { edges = StMap.fold f dfa1.edges StMap.empty;
    start = J (dfa1.start, dfa2.start);
    accept =
      StSet.fold
        (fun s1 cross_prod ->
          StSet.fold
            (fun s2 cross_prod -> StSet.add (J (s1, s2)) cross_prod)
            dfa2.accept
            cross_prod)
        dfa1.accept
        StSet.empty }

let negate dfa = 
  { edges = dfa.edges;
    start = dfa.start;
    accept = 
      let lst = List.filter
        (fun s -> not (StSet.mem s dfa.accept))
        (map fst2 (StMap.bindings dfa.edges)) in
      fold_right StSet.add lst StSet.empty }
      
let nullable (dfa : dfa) : bool = 
  let rec f visited fringe = 
    let state = StSet.choose fringe in
    if StSet.mem state dfa.accept then
      true
    else if StSet.mem state visited then
      f visited (StSet.remove state fringe)
    else 
      let edges = StMap.find state dfa.edges in
      let next_states = StSetExt.from_list 
        (edges.other_chars :: AugCharMapExt.values edges.on_char) in
      f (StSet.add state visited)
        (StSet.remove state (StSet.union next_states fringe)) in
  try
    f StSet.empty (StSet.singleton dfa.start)
  with Not_found -> false (* choose failed above *)

  let overlap dfa1 dfa2 =
    nullable (intersect dfa1 dfa2)

let contains (dfa1 : dfa) (dfa2 : dfa) : bool = 
  let dfa2' = negate dfa2 in
  let dfa3 = intersect dfa1 dfa2' in
  printf "DFAs:\n%s\n%s\n%s\n\n\n%!" (FormatExt.to_string DFA.pp dfa1)
    (FormatExt.to_string DFA.pp dfa2')
    (FormatExt.to_string DFA.pp dfa3);
  not (nullable dfa3)

        
let dfa_of_regex (re : regex) : dfa = 
  let (first_state, follow_tbl, symbol_tbl) = tables_of_regex re in
  make_dfa first_state follow_tbl symbol_tbl


type fsm = dfa
let fsm_of_regex = dfa_of_regex

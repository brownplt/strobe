open Prelude

module H = Hashtbl

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
  | AnyChar

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
  | AnyChar -> 1
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

  let is_accepting l = match l with
    | LeafIn s -> S.mem AugChar.Accept s
    | LeafNotIn s -> not (S.mem AugChar.Accept s)
      
  let is_not_empty l = match l with
    | LeafIn s -> not (S.is_empty s)
    | _ -> true
      
  (* Returns upto three non-overlapping [Leaf]s that cover the same characters
     as [li] and [l2]. *)
  let disjoint_cover (l1 : leaf) (l2 : leaf) : leaf list =
    let lst = match l1, l2 with
      | LeafIn s1, LeafIn s2 -> 
        let s3 = AugCharSet.inter s1 s2 in
        if AugCharSet.is_empty s3 then
          [ l1; l2 ]
        else
          [ LeafIn (AugCharSet.diff s1 s3);
            LeafIn (AugCharSet.diff s2 s3);
            LeafIn s3 ]
      | LeafNotIn s1, LeafNotIn s2 -> [ LeafNotIn (AugCharSet.inter s1 s2) ]
      | LeafIn s1, LeafNotIn s2 (* symmetric to case below *)
      | LeafNotIn s2, LeafIn s1 -> [ LeafNotIn (AugCharSet.diff s2 s1) ]
    in List.filter is_not_empty lst
    
  let is_disjoint (l1 : leaf) (l2 : leaf) : bool = match l1, l2 with
    | LeafIn s1, LeafIn s2 -> S.is_empty (S.inter s1 s2)
    | LeafNotIn s1, LeafNotIn s2 -> false
    | LeafIn s1, LeafNotIn s2
    | LeafNotIn s2, LeafIn s1 -> S.subset s1 s2

  let subset (t1 : t) (t2 : t) : bool = match t1, t2 with
    | LeafIn s1, LeafIn s2 -> AugCharSet.subset s1 s2
    | LeafNotIn s1, LeafNotIn s2 -> AugCharSet.subset s2 s1
    | LeafIn s1, LeafNotIn s2 -> AugCharSet.is_empty (AugCharSet.inter s1 s2)
    | LeafNotIn s1, LeafIn s2 -> AugCharSet.is_empty (AugCharSet.inter s1 s2)
      
  let disjoint_cover_list (overlapped : leaf list) : leaf list =
    let rec loop (disjoint : leaf list) (overlapped : leaf list) = 
      match overlapped with
        | [] -> disjoint
        | leaf :: overlapped -> (* rebind overlapped *)
            (* disjoint' and overlapped' are mutually disjoint *)
          let (disjoint', overlapped') = 
            List.partition (is_disjoint leaf) disjoint in
          match overlapped' with
            | first_overlapped' :: rest_overlapped' ->
              loop 
                ((disjoint_cover leaf first_overlapped') @ disjoint')
                (rest_overlapped' @ overlapped)
            | [] -> loop (leaf :: disjoint') overlapped
    in loop [] overlapped
    
  let complement lbls = match lbls with
    | [LeafNotIn cs] -> LeafIn cs
    | _ -> let lins = List.map (fun lbl -> match lbl with
                                     | LeafIn cs -> cs
                                     | _ -> failwith "Broken label invariant")
                        lbls in
             LeafNotIn (AugCharSetExt.unions lins)


  let intersect lbl1 lbl2 = match lbl1, lbl2 with
    | LeafIn cs1, LeafIn cs2 -> LeafIn (AugCharSet.inter cs1 cs2)
    | LeafIn cs1, LeafNotIn cs2
    | LeafNotIn cs2, LeafIn cs1 -> LeafIn (AugCharSet.diff cs1 cs2)
    | LeafNotIn cs1, LeafNotIn cs2 -> LeafNotIn (AugCharSet.union cs1 cs2)

  let is_empty lbl = match lbl with
    | LeafIn cs -> AugCharSet.is_empty cs
    | LeafNotIn _ -> false

  open FormatExt

  let pp (t : t) = match t with
    | LeafIn s -> AugCharSetExt.p_set AugChar.pp s
    | LeafNotIn s -> horz [ text "complement"; AugCharSetExt.p_set AugChar.pp s ]

end

type dfa = {
  edges: (leaf * st) list StMap.t;
  start: st;
  accept: StSet.t
}

module DFA = struct
  type t = dfa
    
  open FormatExt
  
  let pp t =
    vert
      [ horz [ text "Start: "; St.pp t.start ];
        horz [ text "Accept: "; StSetExt.p_set St.pp t.accept ];
        StMapExt.p_map St.pp 
          (fun lst -> vert
            (map (fun (l,s) -> horz [ Leaf.pp l; St.pp s ]) lst))
          t.edges ]
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
      f !re
    | AnyChar -> f (NotInSet CharSet.empty) in
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
  (first_state, follow_tbl, sym_tbl)

 (* Page 141, Figure 3.44 *)
let make_dfa (first_state : IntSet.t) (follow : follow_tbl) sym  =
  let next_st_num = ref 0 in
  let next_st () = incr next_st_num; S (!next_st_num - 1) in 
  let accept = ref StSet.empty in
  let edges : (leaf * st) list StMap.t ref  = ref StMap.empty in
  let state_names : (IntSet.t, st) H.t = H.create 100 in
  let err_st = next_st () in
  let rec loop (unmarked_states : IntSet.t list) = match unmarked_states with
    | [] -> ()
    | state :: unmarked_states' ->
      (* recurring on the tail of unmarked_states implicitly marks state *)
      let st = H.find state_names state in (* abbreviation for T *)
      (* deviates from the figure *)
      (* followpos(st) *)
      let follow_st = IntSet.fold 
        (fun i s -> IntSet.union follow.(i) s) state IntSet.empty in
      printf "%s is followed by %s\n"
        (FormatExt.to_string (IntSetExt.p_set FormatExt.int) state)
        (FormatExt.to_string (IntSetExt.p_set FormatExt.int) follow_st);
      let leaves = map (fun i -> sym.(i)) (IntSet.elements state) in
      (* leaves are mutually-disjoint non-empty edges out of state *)
      let leaves = Leaf.disjoint_cover_list leaves in
      let more_unmarked_states = ref [] in
      let edges_from_st : (leaf * st) list =
        map
          (fun leaf -> (* variable a in the figure *)
            let next_state =
              IntSet.fold
                (fun i s -> IntSet.union follow.(i) s)
                (IntSet.filter (fun i -> Leaf.subset leaf sym.(i)) state)
                IntSet.empty in
(*              let next_state = (* variable U in the figure *)
                IntSet.filter (fun i -> Leaf.subset leaf sym.(i)) follow_st in *)
            printf "  created edge to %s\n"
              (FormatExt.to_string (IntSetExt.p_set FormatExt.int) next_state);
            let next_st =
              try H.find state_names next_state
              with Not_found ->
                let abbrev = next_st () in
                H.add state_names next_state abbrev;
                more_unmarked_states := next_state :: !more_unmarked_states;
                abbrev in
            if Leaf.is_accepting leaf then
              accept := StSet.add st !accept;
            (leaf, next_st))
          leaves in
      edges := StMap.add st (((Leaf.complement (map fst2 edges_from_st)), 
                              err_st)::(edges_from_st)) !edges;
      loop (!more_unmarked_states @ unmarked_states') in
    (* start of algorithm *)
  let first_st = next_st () in
  printf "first state is %s\n" (FormatExt.to_string (IntSetExt.p_set
                                                       FormatExt.int) first_state);
  H.add state_names first_state first_st;
  loop [ first_state ];
  printf "DFA construction complete!\n";
  { edges = StMap.add err_st [(LeafNotIn AugCharSet.empty, err_st)] !edges;
    start = first_st;
    accept = !accept }
    
let split_list lbls1 lbls2 = 
  let rec slice lbl lbls = match lbl, lbls with
    | lbl1, lbl2::rest -> 
      let inter = Leaf.intersect lbl1 lbl2 in
      if Leaf.is_empty inter 
      then slice lbl rest
      else inter::(slice lbl rest)
    | lbl1, [] -> [] in
  let rec f cs1 cs2 = match cs1, cs2 with
    | cs::rest, _ -> (slice cs cs2)@(f rest cs2)
    | [], _ -> [] in
  f lbls1 lbls2

let intersect dfa1 dfa2 =
  let contains cs1 cs2 : bool = match cs1, cs2 with
    | LeafIn s1, LeafIn s2 -> AugCharSet.subset s1 s2
    | LeafNotIn s1, LeafNotIn s2 -> AugCharSet.subset s2 s1 
    | LeafIn s1, LeafNotIn s2
    | LeafNotIn s2, LeafIn s1 -> AugCharSet.is_empty (AugCharSet.inter s1 s2) in
  let f state1 edges1 edgemap1 =
    let g state2 edges2 edgemap2 =
      (* Given a leaf and a set of edges, find the target state *)
      let target lbl (edges : (leaf * st) list) : st =
        try
          (* The leaf labels must be smaller (since we disjoint them) *)
          snd2 (List.find (fun (lbl', st) -> 
            contains lbl lbl') edges) 
        with Not_found -> failwith "Bad miss in intersect" in
      let cover = split_list (map fst2 edges1) (map fst2 edges2) in
      let new_edges = List.map
        (fun lbl -> lbl, J (target lbl edges1, target lbl edges2)) cover in
      StMap.add (J (state1, state2)) new_edges edgemap2 in
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
  let rec f edges fringe = 
    let next_states state =
      try map snd2 (StMap.find state edges)
      with Not_found -> [] in
    let state = StSet.choose fringe in
    if StSet.mem state dfa.accept then
      true
    else 
      let fringe' = 
        StSet.remove state
          (fold_right StSet.add (next_states state) fringe) in
      f (StMap.remove state edges) fringe'
  in try
       f dfa.edges (StSet.singleton dfa.start)
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

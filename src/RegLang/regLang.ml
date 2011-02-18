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

(* augmented regular expressions *)

module AugChar = struct
  type t = Char of char | Accept

  let compare = Pervasives.compare
end

module AugCharSet = Set.Make (AugChar)

let make_augCharSet (set : CharSet.t) : AugCharSet.t =
  CharSet.fold (fun ch set' -> AugCharSet.add (AugChar.Char ch) set')
    set AugCharSet.empty

module DFA = struct

  type pos = int
      
  type aux = {
    nullable : bool;
    first_pos : IntSet.t;
    last_pos : IntSet.t;
  }


  type augex =
    | AInSet of AugCharSet.t * pos * aux
    | ANotInSet of AugCharSet.t * pos * aux
    | AAlt of augex * augex * aux
    | ACat of augex * augex * aux
    | AStar of augex * aux
    | AEpsilon of aux

  (* augex construction ensures that this is the max. position *)
  let max_pos (arx : augex) = match arx with
    | ACat (_, AInSet (_, p, _), _) -> p
    | _ -> raise (Invalid_argument "ill-formed augex (missing # ?)")

  let aux_of arx = match arx with
    | AInSet (_, _, x) -> x
    | ANotInSet (_, _, x) -> x
    | AAlt (_, _, x) -> x
    | ACat (_, _, x) -> x
    | AStar (_, x) -> x
    | AEpsilon x -> x

  let rec augex_of_regex (re : regex) : augex = 
    let next_pos = ref 0 in
    let pos () = incr next_pos; !next_pos - 1 in

    let rec f (re : regex) : augex = match re with
      | Empty -> AEpsilon ({ nullable = true; 
                             first_pos = IntSet.empty; 
                             last_pos = IntSet.empty })
      | InSet set -> 
        let p = pos () in
        let s = IntSet.singleton p in
        AInSet (make_augCharSet set, p,
                { nullable = true;
                  first_pos = s;
                  last_pos = s })
      | NotInSet set -> 
        let p = pos () in
        let s = IntSet.singleton p in
        ANotInSet (make_augCharSet set, p,
                   { nullable = true;
                     first_pos = s;
                     last_pos = s })
      | Alt (re1, re2) ->
        let arx1 = f re1 in
        let arx2 = f re2 in
        let aux1 = aux_of arx1 in
        let aux2 = aux_of arx2 in
        AAlt (arx1, arx2, 
              { nullable = aux1.nullable || aux2.nullable;
                first_pos = IntSet.union aux1.first_pos aux2.first_pos;
                last_pos = IntSet.union aux1.last_pos aux2.last_pos })
      | Concat (re1, re2) ->
        let arx1 = f re1 in
        let arx2 = f re2 in
        let aux1 = aux_of arx1 in
        let aux2 = aux_of arx2 in
        ACat (arx1, arx2, 
              { nullable = aux1.nullable || aux2.nullable;
                first_pos = 
                  if aux1.nullable then
                    IntSet.union aux1.first_pos aux2.first_pos
                  else 
                    aux1.first_pos;
                last_pos = 
                  if aux2.nullable then
                    IntSet.union aux1.last_pos aux2.last_pos
                  else 
                    aux2.last_pos })
      | Star re' ->
        let arx' = f re' in
        let aux' = aux_of arx' in
        AStar (arx', { nullable = true;
                       first_pos = aux'.first_pos;
                       last_pos = aux'.last_pos })
      (* The remaining variants of regex are inessential *)
      | String str ->
        let re = ref Empty in
        String.iter
          (fun ch -> re := Concat (!re, InSet (CharSet.singleton ch)))
          str;
        f !re
      | AnyChar -> f (NotInSet CharSet.empty) in
    let arx = f re in
    let aux = aux_of arx in
    let accept_pos = pos () in
    let accept_pos_set = IntSet.singleton accept_pos in
    let accept_aux =
      { nullable = false; 
        first_pos = accept_pos_set;
        last_pos = accept_pos_set } in
    let accept_rx =
      AInSet (AugCharSet.singleton AugChar.Accept, accept_pos, accept_aux) in
    (* ACat, knowing that accept_aux is not nullable *)
    ACat (arx, accept_rx,
          { nullable = aux.nullable;
            first_pos = 
              if aux.nullable then
                IntSet.union aux.first_pos accept_aux.first_pos
              else 
                aux.first_pos;
            last_pos = accept_aux.last_pos })


  type follow_tbl  = IntSet.t array

  type lbl =
    | LIn of CharSet.t
    | LNotIn of CharSet.t

  type leaf = 
    | LeafIn of AugCharSet.t 
    | LeafNotIn of AugCharSet.t

  let lbl_of_leaf leaf =
    let unaug set = AugCharSet.fold
      (fun ac set' -> match ac with
        | AugChar.Char ch -> CharSet.add ch set'
        | AugChar.Accept -> set')
      set CharSet.empty in
    match leaf with
      | LeafIn s -> LIn (unaug s)
      | LeafNotIn s -> LNotIn (unaug s)

  type sym_tbl = leaf array

  module Leaf = struct

    type t = leaf

    module S = AugCharSet

    let is_accepting l = match l with
      | LeafIn s -> S.mem AugChar.Accept s
      | _ -> false

    let is_not_empty l = match l with
      | LeafIn s -> not (S.is_empty s)
      | _ -> true

    (* Returns upto three non-overlapping [Leaf]s that cover the same characters
       as [l1] and [l2]. *)
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


  end

  let follow (arx : augex) : follow_tbl * sym_tbl =
    let tbl = Array.make (max_pos arx + 1) IntSet.empty in
    let lbl = Array.make (max_pos arx + 1) (LeafNotIn AugCharSet.empty) in
    let rec f (arx : augex) : unit = match arx with
      | AInSet (set, pos, _) -> lbl.(pos) <- LeafIn set
      | ANotInSet (set, pos, _) -> lbl.(pos) <- LeafNotIn set
      | AAlt (arx1, arx2, _) -> f arx1; f arx2
      | ACat (arx1, arx2, _) ->
        f arx1;
        f arx2;
        let arx2_first_pos = (aux_of arx2).first_pos in
        IntSet.iter
          (fun last ->
            tbl.(last) <- IntSet.union arx2_first_pos tbl.(last))
          (aux_of arx1).last_pos
      | AStar (arx', aux) ->
        f arx';
        let arx_first_pos = aux.first_pos in
        IntSet.iter
          (fun last ->
            tbl.(last) <- IntSet.union arx_first_pos tbl.(last))
          aux.last_pos
      | AEpsilon _ -> () in
    f arx;
    (tbl, lbl)

  type st = 
    | S of int
    | J of st * st (* used for intersection *)


  module St = struct
    type t = st
    let compare = Pervasives.compare
  end 

  module StMap = Map.Make (St)

  module StSet = Set.Make (St)


  type dfa = {
    edges: (lbl * st) list StMap.t;
    start: st;
    accept: StSet.t
  }

  (* Page 141, Figure 3.44 *)
  let make_dfa (arx : augex) (follow : follow_tbl) (sym : sym_tbl)  =
    let next_st_num = ref 0 in
    let next_st () = incr next_st_num; S (!next_st_num - 1) in 
    let accept = ref StSet.empty in
    let edges : (lbl * st) list StMap.t ref  = ref StMap.empty in
    let marked_states : (IntSet.t, st) H.t = H.create 100 in
    let rec loop (unmarked_states : IntSet.t list) = match unmarked_states with
      | [] -> ()
      | state :: unmarked_states' ->
        (* recurring on the tail of unmarked_states implicitly marks state *)
        let st = H.find marked_states state in (* abbreviation for T *)
        (* deviates from the figure *)
        (* followpos(st) *)
        let follow_st = IntSet.fold 
          (fun i s -> IntSet.union follow.(i) s) state IntSet.empty in
        let leaves = map (fun i -> sym.(i)) (IntSet.elements follow_st) in
        (* leaves are mutually-disjoint non-empty edges out of state *)
        let leaves = Leaf.disjoint_cover_list leaves in
        let more_unmarked_states = ref [] in
        let edges_from_st : (lbl * st) list =
          map
            (fun leaf -> (* variable a in the figure *)
              let next_state = (* variable U in the figure *)
                IntSet.filter (fun i -> Leaf.subset leaf sym.(i)) follow_st in
              let next_st =
                try H.find marked_states next_state
                with Not_found ->
                  let abbrev = next_st () in
                  H.add marked_states next_state abbrev;
                  more_unmarked_states := next_state :: !more_unmarked_states;
                  abbrev in
              if Leaf.is_accepting leaf then
                accept := StSet.add st !accept;
              (lbl_of_leaf leaf, next_st))
            leaves in
        edges := StMap.add st edges_from_st !edges;
        loop unmarked_states' in
    (* start of algorithm *)
    let first_state = (aux_of arx).first_pos in
    let first_st = next_st () in
    H.add marked_states first_state first_st;
    loop [ first_state ];
    { edges = !edges;
      start = first_st;
      accept = !accept }

  let intersect dfa1 dfa2 =
    let f state1 edges1 edgemap1 =
      let g state2 edges2 edgemap2 =
        StMap.add (J (state1, state2)) 
          ((map (fun (label, target1) -> 
            (label, J (target1, state2))) edges1)@
              (map (fun (label, target2) -> 
                (label, J (state1, target2))) edges2))
          edgemap2 in
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
      
  let nullable dfa = 
    let next_states state =
      try map snd2 (StMap.find state dfa.edges)
      with Not_found -> [] in
    let rec f edges fringe = 
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

  let contains dfa1 dfa2 = 
    nullable (intersect dfa1 (negate dfa2))
        
  let dfa_of_regex (re : regex) : dfa = 
    let arx = augex_of_regex re in
    let follow_tbl, symbol_tbl = follow arx in
    make_dfa arx follow_tbl symbol_tbl
        
end

type fsm = DFA.dfa

let fsm_of_regex = DFA.dfa_of_regex
let intersect = DFA.intersect
let nullable = DFA.nullable
let contains = DFA.contains

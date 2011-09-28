(*  Copyright (c) 2008-2009, University of Virginia
    All rights reserved.
   
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
       * Redistributions of source code must retain the above copyright
         notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
         copyright notice, this list of conditions and the following
         disclaimer in the documentation and/or other materials
         provided with the distribution.
       * Neither the name of the University of Virginia nor the names 
         of its contributors may be used to endorse or promote products
         derived from this software without specific prior written
         permission.
   
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
    OF THE POSSIBILITY OF SUCH DAMAGE.
   
    Author: Pieter Hooimeijer
*)

(** Nondeterministic finite state automata, implemented using explicit
    Hashtbl representations for delta (transitions that consume a
    symbol) and epsilon (transitions that do not consume a symbol).
*)


module Charset = Dprle_charset
module Hashset = Dprle_hashset
module Options = Dprle_options

open Hashset
(** Constants *)

let def_machine_size    = 500
let def_delta_size      = 50
let def_eps_size        = 50

(** We model characters (i.e. elements of the alphabet) as integers;
    see also {!Charset}.
*)

type charset = Charset.set
type symbol = Character of int
        | Epsilon

(** We model the transition function [delta] as a mapping [state ->
    (state -> character set)]. Epsilon transitions are modeled
    separately by a mapping [state -> state set]:
*)

type 's delta   = ('s, ('s, charset) Hashtbl.t) Hashtbl.t 
type 's epsilon = ('s, 's hashset) Hashtbl.t              
type state = int

(** Nfa with state type ['a], a single start state [s], and a single
    accepting state [f]:
*)
type nfa = {
  mutable s : state;
  mutable f : state;
  mutable delta     : state delta;
  mutable epsilon   : state epsilon;
  mutable q         : state hashset;
  mutable next_q    : state
}

(** {6 Basic transition function handling} *)

(** Get the rhs [(state -> character set)] mapping for a given 
    origin state [s] in transition function [d]

    @param create Whether the inner hashtable should be instantiated if
                  none currently exists
    @param d A transition function 
    @param s Origin state 
    @return A mapping [state -> charset] containing all states
    reachable from [s] by a direct (non-epsilon) transition, with the
    associated set of transition symbols.
*)
let all_delta ?(create = true) 
              (d  : 's delta) 
              (s1 : 's) : ('s, charset) Hashtbl.t =
  try Hashtbl.find d s1
  with Not_found -> 
    if create then 
      let newmap = Hashtbl.create def_delta_size in
  Hashtbl.replace d s1 newmap;
  newmap
    else
      (Hashtbl.create 0)


(** Get the character set by applying [(delta s1) s2]

    @param create Whether the inner hashtable and
                  the inner character set should be created if they do
                  not already exist
    @param delta A transition function
    @param s1 Origin state
    @param s2 Destination state
    @param create Add an empty set if none is found
    @return A [charset] of all non-epsilon transition symbols that
    reach [s2] from [s1] through a direct edge. 
*)
let which_symbols ?(create = true)
                  (d  : 's delta)
                  (s1 : 's)
                  (s2 : 's) : charset =
  let map = all_delta ~create d s1 in
    try Hashtbl.find map s2
    with Not_found ->
      if create then 
  let newset = Charset.create_empty () in
    Hashtbl.replace map s2 newset;
    newset
      else
  (Charset.create_empty ())


(** Get the rhs state set in the epsilon mapping [e]

    @param create Add empty state set if none is found
    @param e An epsilon mapping
    @param s1 An origin state
    @return The set of states reachable from [s1] through
    a single epsilon-transition. If no such states exist,
    an empty set is created.
*)
let which_states ?(create = true) 
                 (e  : 's epsilon) 
                 (s1 : 's) : 's hashset = 
  try Hashtbl.find e s1 
  with Not_found ->
    if create then 
      let newset = Hashset.create def_eps_size in
  Hashtbl.replace e s1 newset;
  newset
    else
      (Hashset.create 0)


(** Copy a delta or epsilon, applying a transformation to each element 
    @param s Source
    @param t Target
    @param f transformation 
*)
let copy_table (s : ('p, ('p, 'q) Hashtbl.t) Hashtbl.t) 
               (t : ('r, ('r, 'q) Hashtbl.t) Hashtbl.t)
               (f : 'p -> 'r) = 
  Hashtbl.iter 
    (fun s1 m -> 
       let old_map = try Hashtbl.find t (f s1) with
     Not_found -> 
       let newmap = Hashtbl.create def_delta_size in
       let _ = Hashtbl.replace t (f s1) newmap in
         newmap 
       in
   Hashtbl.iter
     (fun s2 cs -> 
        Hashtbl.replace old_map (f s2) cs
     ) m
    ) s 

(** For all mappings [p -> q] in hashtable [a] and for all [r -> s] in
    hashtable [b], execute [f p q r s]. Used by {!intersect}
*)
let nested_ht_iter (a : ('a, 'b) Hashtbl.t)
                   (b : ('p, 'q) Hashtbl.t)
                   (f : 'a -> 'b -> 'p -> 'q -> unit) : unit =
  Hashtbl.iter (fun p q -> 
      Hashtbl.iter (fun r s -> f p q r s)  b
         ) a

let fmap set f = 
  let newmap = Hashtbl.create (size set) in
    iter (fun x -> Hashtbl.replace newmap x (f x)) set;
    newmap


(** {6 NFA Construction} *)

(** Create an empty NFA with start state [s] and accepting state [f];
    note that [s] and [f] should not be equal.
    @param s Start state
    @param f Final state
    @return An NFA consisting of states [s] and [f], with no transitions.
*)
let new_nfa_states (s : state) 
                   (f : state) : nfa =
  let d = Hashtbl.create def_machine_size in
  let e = Hashtbl.create def_machine_size in
  let q = create def_machine_size in
    add q s;
    add q f;
    { s = s;
      f = f;
      delta   = d;
      epsilon = e;
      q = q;
      next_q = max (s + 1) (f + 1)
    }


(** Add a new state
    @param n An NFA
    @return The integer id of the state that was added
*)
let new_state (n : nfa) : state =
  let i = n.next_q in
    add n.q i;
    n.next_q <- n.next_q + 1; i


(** Add a specific state
    @param n An NFA
    @param s State id to add
*)
let add_state (n : nfa) (s : state) : unit =
  add n.q s;
  n.next_q <- max n.next_q (s + 1)


(** Add single transition [s1 -c-> s2] 
    @param nfa An NFA
    @param s1 The origin state
    @param c The transition symbol; either a string or epsilon
    @param s2 The destination state
*)
let add_trans (nfa : nfa)
              (s1  : state) 
        (c   : symbol)
        (s2  : state) : unit =
  add_state nfa s1;
  add_state nfa s2;
  match c with
    | Character c -> Charset.add (which_symbols nfa.delta s1 s2) c
    | Epsilon     -> Hashset.add (which_states  nfa.epsilon s1) s2


(** Add transitions [s1 -c-> s2] for all [c] 
    @param nfa An NFA
    @param s1 The origin state
    @param s2 The destination state
*)
let add_all_trans (nfa : nfa)
                  (s1  : state) 
            (s2  : state) : unit =
  add_state nfa s1;
  add_state nfa s2;
  let new_set = Charset.create_full () in
  let curmap = all_delta nfa.delta s1 in
    Hashtbl.replace curmap s2 new_set


(** Add transitions [s1 -c-> s2] for all [c] in [l] 
    @param nfa An NFA
    @param s1 The origin state
    @param cs A (char)set of characters that reach [s2] from [s1]
    @param s2 The destination state
*)
let add_set_trans (nfa : nfa)
                  (s1  : state)
      (cs : charset)
                  (s2  : state) : unit =
  add_state nfa s1;
  add_state nfa s2;
  let cur_set = which_symbols nfa.delta s1 s2 in
  let cur_map = all_delta nfa.delta s1 in
    Hashtbl.replace cur_map s2 (Charset.cup cur_set cs)


(** Construct an NFA that accepts {% $$\Sigma^*$$ %} 
*)
let new_sigmastar () : nfa =
  let newnfa = new_nfa_states 0 1 in
    add_trans newnfa newnfa.s Epsilon newnfa.f;
    add_all_trans newnfa newnfa.f newnfa.f;
    newnfa


(** {6 Basic NFA operations} *)

(** Pretty print an NFA.

    @param nfa NFA to print
*)
let print_nfa (nfa : nfa) : unit =
  let print_trans s1 s2 cset = 
    Printf.printf "q%d -> q%d on " s1 s2;
    Charset.print_charset cset;
    Printf.printf "\n" in
  let print_delta s1 m = 
    Hashtbl.iter (fun s2 cset -> print_trans s1 s2 cset) m in
  let print_eps_trans s1 s2 =
    Printf.printf "q%d -> q%d on epsilon\n" s1 s2 in
  let print_epsilon s1 m = iter (fun s2 -> print_eps_trans s1 s2) m in
    Printf.printf "[ s: q%d f: q%d d: " nfa.s nfa.f;
    Hashtbl.iter print_delta nfa.delta;
    Hashtbl.iter print_epsilon nfa.epsilon;
    Printf.printf "];\n"

(** Convert an NFA to Graphviz dot format 
    
    @param nfa An NFA
    @return A string containing a (currently rather rough)
            dot file representation of the NFA, where
            nodes are states and edges are transitions.
*)
let nfa_to_dot (nfa : nfa) : string = 
  let outb = Buffer.create (Hashtbl.length nfa.delta * 5) in
  let appf x = Printf.bprintf outb x in

  let delta_step q1 rhs =
    let handle_rhs q2 charset = 
      let thecar = Charset.choose charset in
  appf "q%d -> q%d [label=\"%d\"];\n" q1 q2 thecar 
    in 
      Hashtbl.iter handle_rhs rhs in

  let epsilon_step q1 rhs =
    let handle_rhs q2 = appf "q%d -> q%d [ label = \"%s\" ];\n" q1 q2 "eps"
    in iter handle_rhs rhs
  in
    appf "digraph nfa {\nrankdir=LR;\n";
    appf "node [shape = doublecircle]; q%d;" nfa.f;
    appf "node [shape = circle];\n";
    Hashtbl.iter delta_step nfa.delta;
    Hashtbl.iter epsilon_step nfa.epsilon;
    appf "}\n"; Buffer.contents outb


(** Get all immediate outbound neighbors of a given state

    @param nfa An nfa 
    @param q A state in [nfa] 
    @return All states reachable from [s] in [nfa] through a single
    transition ([nfa] is unchanged)
*)
let neighbors (nfa : nfa) 
              (q   : state) :  state list = 
  let d_rhs = all_delta ~create:false nfa.delta q in
  let state_list = Hashtbl.fold (fun state _ acc -> state::acc) d_rhs [] in
  let e_rhs = which_states ~create:false nfa.epsilon q in
  let new_state_list = 
    Hashtbl.fold (fun state _ acc -> state::acc) e_rhs state_list in
    new_state_list

(** We use integer sets for the epsilon closure; this is vaguely
    because we do a lot of comparing between closures *)

module StateSet =
  Set.Make(struct 
       type t = state
       let compare = compare
     end)

type stateset = StateSet.t

(** Epsilon closure for a given NFA state
    @param nfa An NFA
    @param q A state in [nfa]
    @return The set of states transitively reachable through epsilon
            transitions, starting at [q], including [q]
*)
let eps_closure (n : nfa) (q : state) : stateset =
  let visited = ref StateSet.empty in
  let rec walk (queue : state list) : unit = match queue with
    | x::xs when not (StateSet.mem x !visited) ->
  let to_enqueue = to_list (which_states ~create:false n.epsilon x) in
    visited := StateSet.add x !visited;
    walk (List.rev_append xs to_enqueue)
    | x::xs -> walk xs
    | _ -> ()
  in
    walk [q];
    !visited

(** Find all states [q'] such that [q] reaches [q'] on [c]
    through a single transition.
    @param n An NFA
    @param q A state in [n]
    @param c A character
    @return The set of reachable states
*)
let rhs (n : nfa) (q : state) (c : int) : stateset =
  let mapping = all_delta ~create:false n.delta q in
  let res = ref StateSet.empty in
  let process_transition q cs =
    if Charset.mem cs c then res := StateSet.add q !res
  in
    Hashtbl.iter process_transition mapping;
    !res


(** Generic forward walk over an NFA (depth first; visits all states
    forward-reachable from [s]). Not-quite tail-recursive.

    @param f A function [state -> accumulator -> accumulator].
    @param nfa NFA to walk
    @param s State to start the walk from
    @param acc Initial accumulator value
*)
let forward_fold_nfa (f   : state -> 'a -> 'a) 
                     (nfa : nfa) 
         (s   : state)
         (acc : 'a) : 'a = 
  let visited = Hashset.create (size nfa.q) in
  let rec walk acc q = 
    if mem visited q then 
      acc
    else
      begin
  add visited q;
  let acc = f q acc in
    List.fold_left walk acc (neighbors nfa q)
      end
  in
    walk acc s


(* (\** Forward reachability *)
(*     @param nfa An NFA *)
(*     @param s State of interest *)
(*     @return All states that are forward-reachable from [s] in [nfa] *)
(* *\) *)
(* let forward_reachable (nfa : nfa) *)
(*                       (s   : int) : int hashset = *)
(*   let visited = create (size nfa.q) in *)
(*   let rec walk q = *)
(*     match q with  *)
(*       | x::xs when not (mem visited x) -> *)
(*     add visited x; *)
(*     walk (List.rev_append (neighbors nfa x) xs) *)
(*       | x::xs -> walk xs *)
(*       | _ -> () *)
(*   in *)
(*     walk [s]; visited *)

(** Construct a reverse reachability mapping 

    @param nfa an NFA 
    @return A mapping [s -> R] for all states [s] in [nfa.q], where [R]
    contains all the states that have an edge to [s]
*)
let backward_mapping (nfa : nfa) : (state, state hashset) Hashtbl.t =
  let mapping = Hashtbl.create (size nfa.q) in
  let map s1 s2 =
    let theset = try Hashtbl.find mapping s2 with Not_found -> 
      let newset = create (def_delta_size + def_eps_size) in
  Hashtbl.replace mapping s2 newset; newset
    in
      add theset s1
  in
  let add_it s1 s2 _ = map s1 s2 in
    Hashtbl.iter (fun s1 rhs -> Hashtbl.iter (add_it s1) rhs) nfa.delta;
    Hashtbl.iter (fun s1 rhs -> Hashtbl.iter (add_it s1) rhs) nfa.epsilon;
    mapping


(** Backward reachability from a specified state

    @param nfa An NFA
    @param s State of interest
    @return All states that are backward-reachable from [s] in [nfa]
*)
let backward_reachable (nfa : nfa)
                       (s : state) : state hashset =
  let mapping = backward_mapping nfa in
  let mapped x = 
    let neighbors = try Hashtbl.find mapping x with Not_found -> (create 0) in
      fold (fun x acc -> x::acc) neighbors []
  in
  let visited = create (size nfa.q) in
  let rec walk q =
    match q with
      | x::xs when not (mem visited x) ->
    add visited x;
    walk (List.rev_append (mapped x) xs)
      | x::xs -> walk xs
      | _ -> ()
  in
    walk [s]; 
    visited


(** Eliminate states that do not reach the final state. 

    @param nfa An NFA (modified in place)
*)
let elim_dead_states (nfa : nfa) : unit =
  let live_states = backward_reachable nfa nfa.f in
  let dead_lhs = create def_machine_size in
  let dead_rhs = create (def_machine_size * def_delta_size) in
  let dead_eps = create def_machine_size in
  let delta_iter q1 rhs =
    if mem live_states q1 then
      let rhs_iter q2 _ = if not (mem live_states q2) then
  add dead_rhs (q1, rhs, q2) 
      in
  Hashtbl.iter rhs_iter rhs
    else
      add dead_lhs q1
  in
  let epsilon_iter q1 rhs =
    if mem live_states q1 then 
      Hashtbl.replace nfa.epsilon q1 (cap rhs live_states)
    else
      add dead_eps q1
  in
  let do_remove_lhs q = Hashtbl.remove nfa.delta q in
  let do_remove_rhs (q1, tbl, q2) =
    if Hashtbl.length tbl = 1 then 
      Hashtbl.remove nfa.delta q1
    else
      Hashtbl.remove tbl q2
  in
    (* Because Hashtbl dislikes removing elements while iterating, we
       first figure out what to remove and then actually do the
       removal.  *)
    Hashtbl.iter delta_iter nfa.delta;
    Hashtbl.iter epsilon_iter nfa.epsilon;
    nfa.q <- live_states;
    iter do_remove_lhs dead_lhs;
    iter do_remove_rhs dead_rhs

(** Reverse all transitions, swap start and final states; used by 
    {!minimize}

    @param n An NFA (not modified)
    @return A reversed copy of [n] 
*)
let reverse (n : nfa) : nfa =
  let res = new_nfa_states n.f n.s in
  let process_delta q1 q2 cs = add_set_trans res q2 cs q1 in
  let process_eps q1 q2 = add_trans res q2 Epsilon q1 in
  let process_delta_outer q1 rhs = 
    Hashtbl.iter (process_delta q1) rhs in
  let process_eps_outer q1 rhs =
    Hashset.iter (process_eps q1) rhs 
  in
    Hashtbl.iter process_delta_outer n.delta;
    Hashtbl.iter process_eps_outer n.epsilon;
    res


(** Copy an NFA
    
    @param nfa NFA to copy (parameter is unmodified)
    @return a copy of [nfa]
*)
let copy_nfa (nfa : nfa) : nfa = 
  let id x = x in
  let delta   = Hashtbl.create (Hashtbl.length nfa.delta) in
  let epsilon = Hashtbl.create (Hashtbl.length nfa.epsilon) in
  let _ = copy_table nfa.delta delta id in
  let _ = copy_table nfa.epsilon epsilon id in 
    { nfa with delta = delta;
        epsilon = epsilon;
  q = (Hashtbl.copy nfa.q) }
  
(** Extract a subNFA from a bigger NFA
    @param nfa NFA to (partially) copy
    @param s Start state for the result
    @param f Final state for the result
    @return A copy of [nfa] with start state [s], final state [f],
             and a subset of [nfa]'s states
*)
let extract_nfa nfa s f =
  let newnfa = copy_nfa nfa in
  let newfinal = new_state newnfa in
    add_trans newnfa f Epsilon newfinal;
    newnfa.f <- newfinal;
    newnfa.s <- s;
    elim_dead_states newnfa; 
    newnfa


(** Merge two NFAs
    @param target An NFA to "host" the a copy of [source]
    @param source Another NFA (unmodified)
*)
let merge_nfas (target : nfa) (source : nfa) : unit =
  let offset = target.next_q in
  let convert x = x + offset in
    iter (fun source_q -> add target.q (convert source_q)) source.q;
    copy_table source.delta target.delta convert;
    copy_table source.epsilon target.epsilon convert;
    target.next_q <- convert source.next_q


(** Rebuild NFA with contiquous integer state space.

    @param nfa An NFA (not modified)
    @param base Lowest state ID for output machine 
    @return NFA with [base] as the lowest state ID and
                [base + (size nfa.q) - 1] as the highest
*)
let normalize_nfa (nfa : nfa) 
                  (base : int) : nfa =
  let curq = ref base in
  let qmap = Hashtbl.create (size nfa.q) in
  let convert q = try Hashtbl.find qmap q with Not_found -> 
    Hashtbl.replace qmap q !curq;
    incr curq;
    (!curq - 1) in
  let q       = create (size nfa.q) in
  let delta   = Hashtbl.create (Hashtbl.length nfa.delta) in
  let epsilon = Hashtbl.create (Hashtbl.length nfa.epsilon) in
    iter (fun s -> add q (convert s)) nfa.q;
    copy_table nfa.delta delta convert;
    copy_table nfa.epsilon epsilon convert;
    { s = convert nfa.s;
      f = convert nfa.f;
      delta = delta;
      epsilon = epsilon;
      q = q;
      next_q = !curq}

(* (\** Annotated NFA intersection using cross-product construction. *)
(*     @param p1 A subset of [m1.q]  *)
(*     @return A tuple [(p1', m3)] So that [m3] is an NFA with language *)
(*     [L(m3) = L(m1) cap L(m2)]; [p1'] subset [m3.q] is a mapping from *)
(*     states in [p1] to their corresponding states in [m3.q].  of states *)
(*     that correspond to states in [p1]. *)
(* *\) *)
(* let intersect (m1 : nfa)  *)
(*               (m2 : nfa) *)
(*         (p1 : int hashset) : ((int, int list) Hashtbl.t * nfa) = *)
(*   let lhs = Hashtbl.create (size m2.q * size p1) in *)
(*   let put tbl x y =  *)
(*     let list = try Hashtbl.find tbl x with Not_found -> [] in *)
(*       Hashtbl.replace tbl x (y::list) in *)
(*   let cur_id = ref 0 in *)
(*   let newstates = Hashtbl.create ((size m1.q) * (size m2.q)) in *)
(*   let state (x,y) = (try Hashtbl.find newstates (x,y)  *)
(*          with Not_found ->  *)
(*            Hashtbl.replace newstates (x,y) !cur_id; *)
(*            if Hashset.mem p1 x then put lhs x !cur_id; *)
(*            incr cur_id; *)
(*            !cur_id - 1 *)
(*         ) in *)
(*   let result = new_nfa_states (state(m1.s,m2.s)) (state(m1.f,m2.f)) in *)
(*   let process_delta m1q1 map1 m2q1 map2 = *)
(*     nested_ht_iter map1 map2  *)
(*       (fun m1q2 cset1 m2q2 cset2 -> *)
(*    let isect = Charset.cap cset1 cset2 in *)
(*      if not (Charset.empty isect) then *)
(*        add_set_trans result  *)
(*          (state(m1q1,m2q1)) isect (state(m1q2,m2q2))) *)
(*   in *)
(*   let process_eps m1q1 m1qset m2q1 m2qset = *)
(*     nested_ht_iter m1qset m2qset *)
(*       (fun m1q2 _ m2q2 _ ->  *)
(*    add_trans result (state(m1q1,m2q1)) Epsilon (state(m1q2,m2q2))) *)
(*   in *)
(*   let process_eps_left m1q1 m1qset m2q _ = *)
(*     iter (fun m1q2 ->  *)
(*       add_trans result (state(m1q1,m2q)) Epsilon (state(m1q2,m2q)) *)
(*    ) m1qset *)
(*   in *)
(*   let process_eps_right m1q _ m2q1 m2qset = *)
(*     iter (fun m2q2 -> *)
(*       add_trans result (state(m1q,m2q1)) Epsilon (state(m1q,m2q2)) *)
(*    ) m2qset *)
(*   in *)
(*     nested_ht_iter m1.delta m2.delta process_delta; *)
(* (\*    nested_ht_iter m1.epsilon m2.epsilon process_eps; *\) *)
(*     nested_ht_iter m1.epsilon m2.q process_eps_left; *)
(*     nested_ht_iter m1.q m2.epsilon process_eps_right; *)
(*     (lhs, result) *)


(** {6 DFA-related operations} *)

(** Determinize an NFA (necessary for e.g. compliment)
    @param n NFA to turn into a DFA
    @return An NFA that looks mostly like a DFA: all states have
            outbound edges for every alphabet symbol, and no
            epsilon transitions are used *except* for transitions
            to the (single) final state.
*)
let nfa_to_dfa (n : nfa) : nfa = 
  let visited = Hashset.create n.next_q in (* TODO: n.next_q is prob too small...*)
  let statemapping = Hashtbl.create n.next_q in
  let dfa = new_nfa_states 0 0 in
  let sink_state = new_state dfa in
  let cur_q = ref dfa.next_q in
  let convert (q : stateset) : state =
    try Hashtbl.find statemapping q
    with Not_found ->
      let res = !cur_q in
  incr cur_q;
  Hashtbl.replace statemapping q res;
  res
  in (* TODO: some inline comments might help here *)
  let _ = dfa.s <- (convert (eps_closure n n.s)) in
  let bigsigma = Charset.create_full () in
  let _ = add_all_trans dfa sink_state sink_state in
  let process_state (q : stateset) : stateset list = 
    let all_outbound = ref (Charset.create_empty ()) in
    let process_out x = 
      let some_outbound = ref (Charset.create_empty ()) in
      let mapping = all_delta ~create:false n.delta x in
  Hashtbl.iter (fun z y -> 
      some_outbound := Charset.cup !some_outbound y) mapping;
  all_outbound := Charset.cup !all_outbound !some_outbound in
    let _ = StateSet.iter process_out q in
    let not_covered = Charset.minus bigsigma !all_outbound in
    let _ = add_set_trans dfa (convert q) not_covered sink_state in
    let _ = if StateSet.exists ((=) n.f) q then 
      add_trans dfa (convert q) Epsilon dfa.f
    in 
    let res = ref [] in
    let process_symbol s = 
      let rhs_states = ref (StateSet.empty) in
      let process_rhs q_rhs = 
  rhs_states := StateSet.union !rhs_states (eps_closure n q_rhs)
      in
      let process_lhs q_lhs = 
  StateSet.iter process_rhs (rhs n q_lhs s)
      in
  StateSet.iter process_lhs q;
  res := !rhs_states :: !res;
  add_trans dfa (convert q) (Character s) (convert !rhs_states)
    in
      Charset.iter process_symbol !all_outbound;
      !res
  in
  let rec walk (q : stateset list) = match q with
    | x::xs when not (mem visited x) ->
  add visited x;
  let to_enqueue = process_state x in
    walk (List.rev_append xs to_enqueue)
    | x::xs -> walk xs
    | _ -> ()
  in
    walk [(eps_closure n n.s)];
    dfa

(** Compute the complement of a given determinized NFA. Note: Changes
    its parameter in place.
    @param dfa An NFA that has been determinized; this 
               function will fail miserably if it was not.
*)
let complement (dfa : nfa) : unit =
  let newfinal = new_state dfa in
    iter (fun q ->
      let rhs = which_states ~create:false dfa.epsilon q in
        if empty rhs then
    add_trans dfa q Epsilon newfinal
        else 
    Hashtbl.remove dfa.epsilon q) dfa.q;
    Hashtbl.remove dfa.epsilon dfa.f;
    Hashtbl.remove dfa.epsilon newfinal;
    dfa.f <- newfinal


(** Minimize an NFA using Brzozowski's minimization algorithm
    @param n NFA to minimize
    @return A minimized copy of [n]
*)
let minimize (n : nfa) : nfa =
  nfa_to_dfa (reverse (nfa_to_dfa (reverse n)))


(** {6 Language Operations} *)

(** Annotated NFA concatenation using a single epsilon-transition.
    @param p1 A subset of [m1.q] 
    @param p2 A subset of [m2.q]
    @return A tree-tuple [(p1', p2', m3)] So that [m3] is an NFA with
    language [L(m3) = L(m1) concat L(m2)]; [p1'] subset [m3.q] is the
    set of states that correspond to states in [p1]; and [p2'] subset
    [m3.q] is the set of states that correspond to states in
    [p2]. (Note: for the current concat implementation, [p1 = p1'] for
    all calls to concat).
    
*)
let concat (a  : nfa)
           (b  : nfa)  
           (p1 : state hashset) 
           (p2 : state hashset) : ((state, state) Hashtbl.t * 
         (state, state) Hashtbl.t * nfa) =
  let offset = a.next_q in
  let convert x = x + offset in
  let lhs = fmap p1 (fun x -> x) in
  let rhs = fmap p2 convert in
  let q = copy a.q in
  let _ = iter (fun m2q -> add q (convert m2q)) b.q in
  let delta = Hashtbl.create (Hashtbl.length a.delta + Hashtbl.length b.delta) in
  let _ = copy_table a.delta delta (fun x -> x) in
  let _ = copy_table b.delta delta convert in
  let epsilon = Hashtbl.create (Hashtbl.length a.delta + Hashtbl.length b.delta) in
  let _ = copy_table a.epsilon epsilon (fun x -> x) in
  let _ = copy_table b.epsilon epsilon convert in
  let result = { s = a.s; f = (convert b.f); delta = delta; epsilon = epsilon; q = q;
           next_q = a.next_q + b.next_q } in
  let curset = which_states epsilon a.f in
  let _ = add curset (convert b.s) in
    (lhs, rhs, result)

(** Concat without the state-converting frills

    @param m1 An NFA (unmodified)
    @param m2 Another NFA (unmodified)
    @return An NFA that accepts [L(m1).L(m2)]
*)
let simple_concat (m1 : nfa)
                  (m2 : nfa) : nfa =
  let _,_,m = concat m1 m2 (create 0) (create 0) in m


(** Applicative NFA union
    @param a An NFA (unchanged)
    @param b Another NFA (unchanged)
    @return An NFA that accepts [L(a) cup L(b)]
*)
let union (a : nfa)
          (b : nfa) : nfa =
  let offset = a.next_q in
  let convert x = x + offset in
  let q = copy a.q in
  let _ = iter (fun m2q -> add q (convert m2q)) b.q in
  let delta = Hashtbl.create (Hashtbl.length a.delta + Hashtbl.length b.delta) in
  let _ = copy_table a.delta delta (fun x -> x) in
  let _ = copy_table b.delta delta convert in
  let epsilon = Hashtbl.create (Hashtbl.length a.delta + Hashtbl.length b.delta) in
  let _ = copy_table a.epsilon epsilon (fun x -> x) in
  let _ = copy_table b.epsilon epsilon convert in
  let result = { s = 0; f = 0; delta = delta; epsilon = epsilon; q = q;
           next_q = a.next_q + b.next_q } in
  let newstart = new_state result in
  let newfinal = new_state result in
    add_trans result newstart Epsilon a.s;
    add_trans result newstart Epsilon (convert b.s);
    add_trans result a.f Epsilon newfinal;
    add_trans result (convert b.f) Epsilon newfinal;
    result.s <- newstart;
    result.f <- newfinal;
    result


(** Annotated NFA intersection using the cross-product construction
    @param p1 A subset of [m1.q]
    @return A tuple [(p1', m3)] So that [m3] is an NFA with language
    [L(m3) = L(m1) cap L(m2)]; [p1'] is a mapping from
    states in [p1] to their corresponding states in [m3.q]
*)
let intersect (m1 : nfa) 
              (m2 : nfa)
        (p1 : state hashset) : ((state, state list) Hashtbl.t * nfa) =
  let m1 = (if !Options.maxsize > 0 && size m1.q > !Options.maxsize then 
        minimize m1 else  m1) in
  let m2 = (if !Options.maxsize > 0 && size m2.q > !Options.maxsize then 
        minimize m2 else m2) in
    
  let lhs = Hashtbl.create (size m2.q * size p1) in
  let put tbl x y = 
    let list = try Hashtbl.find tbl x with Not_found -> [] in
      Hashtbl.replace tbl x (y::list) in

  let cur_id = ref 0 in
  let queue  = ref [] in
  let newstates = Hashtbl.create (size m1.q) in

  let state (x,y) = (try Hashtbl.find newstates (x,y) 
         with Not_found -> 
           queue := (x,y)::(!queue);
           Hashtbl.replace newstates (x,y) !cur_id;
           if Hashset.mem p1 x then put lhs x !cur_id;
           incr cur_id;
           !cur_id - 1
        ) in

  let result = new_nfa_states (state(m1.s,m2.s)) (state(m1.f,m2.f)) in

  let step (q1, q2) = 
    let delta_step q1' cset1 q2' cset2 =
      let charset = Charset.cap cset1 cset2 in
  if not (Charset.empty charset) then
    add_set_trans result (state (q1,q2)) charset (state (q1',q2')) 
    in
    let left_eps_step m1rhs =
      iter (fun m1q2 -> 
        Hashtbl.iter (fun (a,b) y -> 
            if a = q1 then
        let newstate = state (m1q2, b) in
          add_trans result y Epsilon newstate
         ) newstates
     ) m1rhs
    in
    let right_eps_step m2rhs =
      iter (fun m2q2 -> 
        Hashtbl.iter (fun (a,b) y -> 
            if b = q2 then
        let newstate = state (a, m2q2) in
          add_trans result y Epsilon newstate
         ) newstates
     ) m2rhs
    in

    let map1 = all_delta ~create:false m1.delta q1 in
    let map2 = all_delta ~create:false m2.delta q2 in
    let eps1 = which_states ~create:false m1.epsilon q1 in
    let eps2 = which_states ~create:false m2.epsilon q2 in
      nested_ht_iter map1 map2 delta_step;
      left_eps_step  eps1;
      right_eps_step eps2 in

  let rec walk () = match !queue with
    | (q1,q2)::qs -> queue := qs; step (q1,q2); walk ()
    | _ -> () in

    queue := [ m1.s, m2.s ];
    walk ();
    (lhs, result)


(** Intersection without the state-converting frills
*)
let simple_intersect (m1 : nfa)
                     (m2 : nfa) : nfa =
  let _,m = intersect m1 m2 (create 0) in m


exception Found_it

(** Language emptiness
    @param nfa NFA to check for emptiness
    @return [true] if [nfa] accepts no strings; [false] otherwise
*)
let is_empty (nfa : nfa) : bool = 
  try
    forward_fold_nfa (fun n acc -> 
      if (n = nfa.f) then raise Found_it else acc
         ) nfa nfa.s true
  with Found_it -> false


(** Language equality
    @param a An NFA (unmodified)
    @param b Another NFA (also unmodified)
    @return [true] if [L(a) = L(b)]; false otherwise
*)
let nfa_eq (a : nfa) (b : nfa) : bool = 
  let abar = nfa_to_dfa a in
  let _ = complement abar in
  let bbar = nfa_to_dfa b in
  let _ = complement bbar in
  let cap = simple_intersect in
  let cup = union in
  let big = cup (cap a bbar) (cap abar b) in
    is_empty big

(** Language subseteq
    @param a An NFA (unmodified)
    @param b Another NFA (also unmodified)
    @return [true] if [L(a) subseteq L(b)]; false otherwise
*)
let nfa_subseteq (a : nfa) (b : nfa) : bool =
  let bbar = nfa_to_dfa b in
  let _ = complement bbar in
  let res = simple_intersect a bbar in
    is_empty res 


let gen_string (m : nfa) : string option =
  let visited = create (size m.q) in
  let visit q = add visited q in
  let visited q = mem visited q in

  let get_next (w : string) (q : state) =
    let res = ref [] in
    let epsset = eps_closure m q in
    let delta = all_delta ~create:false m.delta q in
    let handle_eps q2 =
      res := (w, q2)::!res 
    in
    let handle_rhs q2 charset =
      let handle_char c = 
  res := (w ^ (Charset.char_as_string c), q2)::!res in
      Charset.iter handle_char charset 
    in
      StateSet.iter handle_eps epsset;
      Hashtbl.iter handle_rhs delta;
      !res in

  let rec walk (queue: (string * state) list) : string option = match queue with
    | (w, q)::qs when q = m.f -> Some w
    | (w, q)::qs when not (visited q) -> visit q; walk (qs @ (get_next w q))
    | (w, q)::qs -> walk qs
    | [] -> None 
  in
    walk [("", m.s)]
   

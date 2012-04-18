open Prelude

open Dprle_nfa
open RegLang_syntax


let set_to_set (set : CharSet.t) : charset = 
  let set' = Dprle_charset.create_empty () in
  let ins ch = Dprle_charset.add set' (Char.code ch) in
  CharSet.iter ins set;
  set'

let set_to_set_complement (set : CharSet.t) : charset = 
  let set' = Dprle_charset.create_full () in
  let ins ch = Dprle_charset.remove set' (Char.code ch) in
  CharSet.iter ins set;
  set'


let rec mk (nfa : nfa) (rx : regex) (start : state) (stop : state) : nfa =
  let connect_and_offset offset nfa =
    let ret = new_nfa_states start stop in
    merge_nfas ret nfa;
    add_trans ret start Epsilon (nfa.s + offset);
    add_trans ret (nfa.f + offset) Epsilon stop;
    ret in
  
  match rx with
    | InSet set -> add_set_trans nfa start (set_to_set set) stop; nfa
    | NotInSet set -> add_set_trans nfa start (set_to_set_complement set) stop; nfa
    | Alt (rx1, rx2) ->
      let nfa = mk nfa rx1 start stop in
      mk nfa rx2 start stop
    | Star rx' ->
      let nfa = mk nfa rx' start stop in
      add_trans nfa start Epsilon stop;
      add_trans nfa stop Epsilon start;
      nfa
    | Empty -> add_trans nfa start Epsilon stop; nfa
    | Concat (rx1, rx2) ->
      let mid = new_state nfa in
      let nfa = mk nfa rx1 start mid in
      mk nfa rx2 mid stop
    | String str -> 
      let rx' = ref Empty in
      String.iter
        (fun ch -> rx' := Concat (!rx', InSet (CharSet.singleton ch)))
        str;
      mk nfa !rx' start stop
    | Negate rx' -> 
      let next = nfa.next_q in
      let rx'_nfa = new_nfa_states 0 1 in
      let rx'_nfa = mk rx'_nfa rx' 0 1 in
      let rx'_dfa = minimize rx'_nfa in
      complement rx'_dfa;
      connect_and_offset next rx'_dfa
    | Inter (rx1, rx2) ->
      let next = nfa.next_q in
      let nfa1 = mk (new_nfa_states 0 1) rx1 0 1 in
      let nfa2 = mk (new_nfa_states 0 1) rx2 0 1 in
      let inter = simple_intersect nfa1 nfa2 in
      connect_and_offset next inter
    | Subtract (rx1, rx2) -> mk nfa (Inter(rx1, Negate(rx2))) start stop

      
let rec to_nfa (rx : regex) : nfa = 
  let start = 0 in
  let stop = 1 in
  let nfa = new_nfa_states start stop in
  mk nfa rx start stop


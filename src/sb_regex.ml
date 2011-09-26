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


let rec mk (nfa : nfa) (rx : regex) (start : state) (stop : state) : unit =
 
  match rx with
    | InSet set -> add_set_trans nfa start (set_to_set set) stop
    | NotInSet set -> add_set_trans nfa start (set_to_set_complement set) stop
    | Alt (rx1, rx2) ->
      mk nfa rx1 start stop;
      mk nfa rx2 start stop
    | Star rx' ->
      mk nfa rx' start stop;
      add_trans nfa start Epsilon stop;
      add_trans nfa stop Epsilon start
    | Empty -> add_trans nfa start Epsilon stop
    | Concat (rx1, rx2) ->
      let mid = new_state nfa in
      mk nfa rx1 start mid;
      mk nfa rx2 mid stop
    | String str -> 
      let rx' = ref Empty in
      String.iter
        (fun ch -> rx' := Concat (!rx', InSet (CharSet.singleton ch)))
        str;
      mk nfa !rx' start stop
    | Negate _ -> failwith "trying to construct from Negate regex"

      
let rec to_nfa (rx : regex) : nfa = 
  let start = 0 in
  let stop = 1 in
  let nfa = new_nfa_states start stop in
  mk nfa rx start stop;
  nfa

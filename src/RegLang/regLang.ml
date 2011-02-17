open Prelude

module H = Hashtbl

module CharSet = Set.Make (Char)

type regex =
  | InSet of CharSet.t
  | NotInSet of CharSet.t
  | Alt of regex * regex
  | Star of regex
  | Empty
  | String of string
  | Concat of regex * regex
  | AnyChar

type label =
  | Epsilon
  | In of CharSet.t
  | NotIn of CharSet.t

type state = 
    | I of int
    | II of (state * state)

module State = struct
  type t = state
  let compare = Pervasives.compare
end

module StateMap = Map.Make (State)
module StateMapExt = MapExt.Make (State) (StateMap)

type fsm = {
  edges: (label * state) list StateMap.t;
  start: state;
  accept: state;
}

let join_err _ _ _ = failwith "overlapping states"

let nfa_of_regex (re : regex) : fsm =
  let free_state = ref 0 in
  let make_state () = 
    incr free_state;
    I (!free_state - 1) in
  let rec f  re = match re with
    | Concat (re1, re2) ->
      let fsm1 = f re1 in
      let fsm2 = f re2 in
      { 
        edges = StateMap.add fsm1.accept [(Epsilon, fsm2.start)]
          (StateMapExt.join join_err fsm1.edges fsm2.edges);
        start = fsm1.start;
        accept = fsm2.accept
      }
    | InSet set -> 
      let start = make_state () in
      let accept = make_state () in
      {
        edges = StateMap.singleton start [(In set, accept)];
        start = start;
        accept = accept
      }
    | NotInSet set ->
      let start = make_state () in
      let accept = make_state () in
      {
        edges = StateMap.singleton start [(NotIn set, accept)];
        start = start;
        accept = accept
      }
    | Star re' ->
      let fsm' = f re' in
      let accept = make_state () in
      { 
        edges =
          StateMap.add fsm'.accept [(Epsilon, fsm'.start)]
            (StateMap.add fsm'.accept [(Epsilon, accept)] fsm'.edges);
        start = fsm'.start;
        accept = accept
      }
    | Alt (re1, re2) ->
      let fsm1 = f re1 in
      let fsm2 = f re2 in
      let start = make_state () in
      let accept = make_state () in
      {
        edges =
          StateMap.add start [(Epsilon, fsm1.start); (Epsilon, fsm2.start)]
            (StateMap.add fsm1.accept [(Epsilon, accept)]
               (StateMap.add fsm2.accept [(Epsilon, accept)]
                  (StateMapExt.join join_err fsm1.edges fsm2.edges)));
        start = start;
        accept = accept
      }
    | Empty ->
      let start = make_state () in
      let accept = make_state () in
      { 
        edges = StateMap.singleton start [(Epsilon, accept)];
        start = start;
        accept = accept
      }
    | String str ->
      let re = ref Empty in
      String.iter
        (fun ch -> re := Concat (!re, InSet (CharSet.singleton ch)))
        str;
      f !re
    | AnyChar -> f (NotInSet CharSet.empty)
  in f re

let intersect fsm1 fsm2 =
  let f state1 edges1 edgemap1 =
    let g state2 edges2 edgemap2 =
      StateMap.add (II (state1, state2)) 
        ((map (fun (label, target1) -> 
          (label, II (target1, state2))) edges1)@
            (map (fun (label, target2) -> 
              (label, II (state1, target2))) edges2))
        edgemap2 in
    StateMap.fold g fsm2.edges edgemap1 in
  {
    edges = StateMap.fold f fsm1.edges StateMap.empty;
    start = II (fsm1.start, fsm2.start);
    accept = II (fsm1.accept, fsm2.accept);
  }

let nullable fsm = 
  let rec f edges fringe = match fringe with
    | [] -> false
    | state :: rest ->
      if state = fsm.accept then
        true
      else 
        f (StateMap.remove state edges)
          (rest @ (map snd2 (StateMap.find state edges)))
  in f fsm.edges [fsm.start]
    

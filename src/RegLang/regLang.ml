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

type label =
  | Epsilon
  | In of CharSet.t
  | NotIn of CharSet.t

type state = 
    | I of int
    | II of (state * state)

module Label = struct
  type t = label

  open FormatExt
  let pp t = match t with
    | Epsilon -> text "epsilon"
    | In set -> CharSetExt.p_set (fun ch fmt -> Format.pp_print_char fmt ch) set
    | NotIn set -> 
      sep [ text "not"; 
            CharSetExt.p_set (fun ch fmt -> Format.pp_print_char fmt ch) set ]
end

module State = struct
  type t = state
  let compare = Pervasives.compare

  open FormatExt
  let rec pp t = match t with
    | I n -> int n
    | II (s1, s2) -> angles (squish [ pp s1; text ","; pp s2 ])

end

module StateMap = Map.Make (State)
module StateMapExt = MapExt.Make (State) (StateMap)
module StateSet = Set.Make (State)

type fsm = {
  edges: (label * state) list StateMap.t;
  start: state;
  accept: state;
}

module FSM = struct
  type t = fsm

  open FormatExt

  let pp t =
    vert
      [ horz [ text "Start: "; State.pp t.start ];
        horz [ text "Accept: "; State.pp t.accept ];
        StateMapExt.p_map State.pp 
          (fun lst -> vert
            (map (fun (l,s) -> horz [ Label.pp l; State.pp s ]) lst))
          t.edges ]

end

(* Returns the empty list if [state] is not a state. *)
let next_states (fsm : fsm) (state : state) : state list =
  try
    (map snd2 (StateMap.find state fsm.edges))
  with Not_found -> []

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
      begin match !re with
        | Concat (Empty, re) -> f re (* optimization *)
        | re -> f re
      end

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

let negate fsm = 
  (* I think the accept state is always the highest state,
   * which makes this perfectly "accept"able (ha?) *)
  let rec new_accept accept = match accept with
    | I i -> I (1 + i) 
    | II (s1, s2) -> II (new_accept s1, new_accept s2) in
  let accept' = new_accept fsm.accept in
  let add_accept_edge edges = (Epsilon, accept')::edges in
  {
    edges = StateMap.map add_accept_edge fsm.edges;
    start = fsm.start;
    accept = accept'
  }


let nullable fsm = 
  printf "%s\n\n" (FormatExt.to_string FSM.pp fsm);
  let rec f edges fringe = 
    let state = StateSet.choose fringe in
    if state = fsm.accept then
      true
    else 
      let fringe' = 
        StateSet.remove state
          (fold_right StateSet.add (next_states fsm state) fringe) in
      f (StateMap.remove state edges) fringe'
  in try
       f fsm.edges (StateSet.singleton fsm.start)
    with Not_found -> false (* choose failed above *)

(* TODO: Idea---use the highest "accept" number to decide which
 * to negate as an optimization to minimize edges *)
let contains fsm1 fsm2 =
  printf "%s\n\n" (FormatExt.to_string FSM.pp fsm1);
  printf "%s\n\n" (FormatExt.to_string FSM.pp (negate fsm2));
  nullable (intersect fsm1 (negate fsm2))

let reg_contains field1 field2 = match field1, field2 with
  | (_, fsm1), (_, fsm2) -> contains fsm1 fsm2


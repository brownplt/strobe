module Set : Sig.SET = struct

  open Prelude
  open RegLang
    
  open Dprle_nfa
  open Sb_regex
    
  module Nfa = Dprle_nfa

  type  t = 
    | Pat of Dprle_nfa.nfa
    | Var of Id.t
    | Union of t * t
    | Inter of t * t
    | Diff of t * t
    | Not of t
    | Empty
    | All 

  let uniq_empty = new_nfa_states 0 1

  let uniq_all = new_sigmastar ()

  let rec simpl (t : t) : t = match t with
    | Pat r -> t
    | Var x -> t
    | Empty -> Pat uniq_empty
    | All -> Pat uniq_all
    | Union (t1, t2) -> begin match simpl t1, simpl t2 with
	| Pat r1, Pat r2 -> Pat (union r1 r2)
	| t1', t2' -> Union (t1', t2')
    end
    | Inter (t1, t2) -> begin match simpl t1, simpl t2 with
	| Pat r1, Pat r2 -> Pat (simple_intersect r1 r2)
	| t1', t2' -> Inter (t1', t2')
    end
    | Diff (t1, t2) -> begin match simpl t1, simpl t2 with
	| Pat r1, Pat r2 -> 
	  let r3 = nfa_to_dfa r2 in
	  complement r3;
	  Pat (simple_intersect r1 r3)
	| t1', t2' -> Inter (t1', t2')
    end
    | Not t' -> begin match simpl t' with
	| Pat r ->
	  let r' = nfa_to_dfa r in
	  complement r';
	  Pat r'
	| t'' -> Not t''
    end


  let to_nfa t = match t with
    | Pat r -> r
    | _ -> failwith "expected reduced pattern"

  let singleton str = Pat (Sb_regex.to_nfa (RegLang_syntax.String str))

  let singleton_string t = match t with
    | Pat r -> gen_string r
    | _ -> None


  let intersect t1 t2 = simpl (Inter (t1, t2))

  let union t1 t2 = simpl (Union (t1, t2))

  let negate t = simpl (Not t)

  let subtract t1 t2 = simpl (Diff (t1, t2))

  let var x = Var (Id.id_of_string x)

  let all = Pat uniq_all

  let empty = Pat uniq_empty

  let pretty pat =  "** unprintable DFA **"




  let is_overlapped t1 t2 = 
    t1 == t2 || not (is_empty (simple_intersect (to_nfa t1) (to_nfa t2)))

  let is_subset t1 t2 =
    t1 == t2 || nfa_subseteq (to_nfa t1) (to_nfa t2)

  let is_equal t1 t2 = 
    t1 == t2 || Nfa.nfa_eq (to_nfa t1) (to_nfa t2)

  let is_empty t = is_empty (to_nfa t)
      
  let example t = gen_string (to_nfa t)

  (** Parse a string representing a pattern. *)
  let parse pos str =
    Pat (Sb_regex.to_nfa (RegLang.parse_regex pos str))

end

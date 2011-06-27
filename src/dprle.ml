module Set : Sig.SET = struct

  open Prelude
    
  module P = Sb_strPat 
  open P

  type  t = 
    | Pat of P.t
    | Var of Id.t
    | Union of t * t
    | Inter of t * t
    | Diff of t * t
    | Not of t
    | Empty
    | All 

  let rec prf t =
    let open FormatExt in
    match t with
      | Pat p -> text (P.pretty p)
      | Var x -> text (Id.string_of_id x)
      | Union (t1, t2) -> parens (horz [ text "Union"; prf t1; prf t2 ])
      | Inter (t1, t2) -> parens (horz [ text "Inter"; prf t1; prf t2 ])
      | Diff (t1, t2) -> parens (horz [ text "Diff"; prf t1; prf t2 ])
      | Not t' -> parens (horz [ text "Not"; prf t' ])
      | Empty -> text "Empty"
      | All -> text "All"

  let pretty pat =  FormatExt.to_string prf pat

  let uniq_empty = P.empty

  let uniq_all = P.all

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
	| Pat r1, Pat r2 -> Pat (intersect r1 r2)
	| t1', t2' -> Inter (t1', t2')
    end
    | Diff (t1, t2) -> begin match simpl t1, simpl t2 with
	| Pat r1, Pat r2 -> Pat (subtract r1 r2)
	| t1', t2' -> Inter (t1', t2')
    end
    | Not t' -> begin match simpl t' with
	| Pat r -> Pat (negate r)
	| t'' -> Not t''
    end


  let to_nfa t = match t with
    | Pat r -> r
    | _ -> failwith ("expected reduced pattern, got " ^ pretty t)

  let singleton str = Pat (P.singleton str)

  let singleton_string t = match t with
    | Pat r -> P.singleton_string r
    | _ -> None


  let intersect t1 t2 = simpl (Inter (t1, t2))

  let union t1 t2 = simpl (Union (t1, t2))

  let negate t = simpl (Not t)

  let subtract t1 t2 = simpl (Diff (t1, t2))

  let var x = Var (Id.id_of_string x)

  let all = Pat uniq_all

  let empty = Pat uniq_empty


  let rec is_subset_exn t1 t2 = match (t1, t2) with
    | Pat p1, Pat p2 -> P.is_subset p1 p2
    | Var _, Pat _ -> false
    | Var x, Var y -> Id.compare x y = 0
    | Var x, Diff (t2', Var y) -> false
    | _ -> false
    | t1, t2 ->
      failwith ("is_subset_exn " ^ pretty t1 ^ " " ^ pretty t2)

  let is_overlapped t1 t2 = 
    t1 == t2 || P.is_overlapped (to_nfa t1) (to_nfa t2)

  let is_subset t1 t2 =
    t1 == t2 || is_subset_exn t1 t2 (* P.is_subset (to_nfa t1) (to_nfa t2) *)

  let is_equal t1 t2 = 
    t1 == t2 || P.is_equal (to_nfa t1) (to_nfa t2)

  let is_empty t = P.is_empty (to_nfa t)
      
  let example t = P.example (to_nfa t)

  (** Parse a string representing a pattern. *)
  let parse pos str = Pat (P.parse pos str)

end

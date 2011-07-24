open Prelude
open Sig

module L = ListExt



module Make (Pat : SET) : (TYP with module Pat = Pat) = struct

  exception Typ_error of pos * string

	let num_typ_errors = ref 0

  let error_on_mismatch = ref false

  let with_typ_exns thunk = 
    let prev = !error_on_mismatch in
    error_on_mismatch := true;
    let r = thunk () in
    error_on_mismatch := prev;
    r

	let typ_mismatch p s = 
    if !error_on_mismatch then
      raise (Typ_error (p, s))
    else
      begin
		    incr num_typ_errors;
		    eprintf "%s : %s\n" (string_of_position p) s
      end

	let get_num_typ_errors () = !num_typ_errors


  module P = Pat
  module Pat = Pat
  type pat = P.t

  type prim =
    | Num
    | Int
    | True
    | False
    | Undef
    | Null

  type kind = 
    | KStar
    | KArrow of kind list * kind
	
  type typ = 
    | TPrim of prim
    | TUnion of typ * typ
    | TIntersect of typ * typ
    | TArrow of typ list * typ
    | TObject of obj_typ
    | TRegex of pat
    | TRef of typ
    | TSource of typ
    | TSink of typ
    | TTop
    | TBot
    | TForall of id * typ * typ (** [TForall (a, s, t)] forall a <: s . t *)
    | TId of id
    | TRec of id * typ 
    | TLambda of (id * kind) list * typ (** type operator *)
    | TApp of typ * typ list (** type operator application *)
    | TFix of id * kind * typ (** recursive type operators *)

  and obj_typ = { 
    fields : (pat * prop) list;
    cached_parent_typ : typ option option ref;
    cached_guard_pat : pat option ref
  }
      
  and prop = 
    | PInherited of typ
    | PPresent of typ
    | PMaybe of typ
    | PAbsent


  type field = pat * prop

  type typenv = (typ * kind) IdMap.t

  (* Necessary for equi-recursive subtyping. *)
  module TypPair = struct
    type t = typ * typ
    let compare = Pervasives.compare
  end
  module TPSet = Set.Make (TypPair)
  module TPSetExt = SetExt.Make (TPSet)

  module Pretty = struct

    open Format
    open FormatExt
      
    let rec kind k = match k with
      | KStar -> text "*"
      | KArrow (ks, k) -> 
	horz [horz (intersperse (text ",") (map pr_kind ks)); text "=>"; kind k]

    and pr_kind k = match k with
      | KArrow _ -> parens (kind k)
      | _ -> kind k

    let rec typ t  = match t with
      | TTop -> text "Any"
      | TBot -> text "DoesNotReturn"
      | TPrim p -> text begin match p with
	  | Num -> "Num"
	  | Int -> "Int"
	  | True -> "True"
	  | False -> "False"
	  | Null -> "Null"
	  | Undef -> "Undef"
      end
      | TLambda (args, t) -> 
	let p (x, k) = horz [ text x; text "::"; kind k ] in
	horz [ text "Lambda "; horz (map p args); text "."; typ t ]
      | TFix (x, k, t) -> 
	horz [ text "Fix "; text x; text "::"; kind k; typ t ]
      | TApp (t, ts) ->
	horz [typ t; text "<"; horz (intersperse (text ",") (map typ ts));
	      text ">"]
      | TRegex pat -> 
        squish [text "/"; text (P.pretty pat); text "/"]
      | TUnion (t1, t2) -> horz [typ t1; text "+"; typ t2]
      | TIntersect (t1, t2) -> horz [typ t1; text "&"; typ t2]
      | TArrow (tt::arg_typs, r_typ) ->
        horz[ brackets (typ tt);
              horz (intersperse (text "*") 
                      (map (fun at -> begin match at with
                        | TArrow _ -> parens (typ at)
                        | _ -> typ at 
                      end) arg_typs));
              text "->";
              typ r_typ ]
      | TArrow (arg_typs, r_typ) ->
	horz[ horz (intersperse (text "*") 
                      (map (fun at -> begin match at with
                        | TArrow _ -> parens (typ at)
                        | _ -> typ at 
                      end) arg_typs));
              text "->";
              typ r_typ ]
      | TObject flds -> braces (vert (map pat (flds.fields)))
      | TRef s -> horz [ text "Ref"; parens (typ s) ]
      | TSource s -> horz [ text "Src"; parens (typ s) ]
      | TSink s -> horz [ text "Snk"; parens (typ s) ]
      | TForall (x, s, t) -> 
        horz [ text "forall"; text x; text "<:"; typ s; text "."; typ t ]
      | TId x -> text x
      | TRec (x, t) -> horz [ text "rec"; text x; text "."; typ t ]
	
    and pat (k, p) = horz [ text (P.pretty k); text ":"; prop p;
			    text "," ]
      
    and prop p = match p with
      | PPresent t -> typ t
      | PMaybe t -> horz [ text "maybe"; typ t ]
      | PInherited t -> squish [ text "^"; typ t]
      | PAbsent -> text "_"
	
  end

  let string_of_typ = FormatExt.to_string Pretty.typ
  let string_of_prop = FormatExt.to_string Pretty.prop
  let string_of_kind = FormatExt.to_string Pretty.kind


  let proto_str = "__proto__"
    
  let proto_pat = P.singleton proto_str

      
  let mk_obj_typ (fs: field list) : obj_typ = 
    { fields = fs;
      cached_parent_typ = ref None;
      cached_guard_pat = ref None }

  let fields ot = ot.fields

  let prop_typ prop = match prop with
    | PInherited t
    | PPresent t
    | PMaybe t -> Some t
    | PAbsent -> None

  let rec free_typ_ids (typ : typ) : IdSet.t =
    let open IdSet in
    let open IdSetExt in
    match typ with
      | TTop
      | TBot
      | TPrim _ 
      | TRegex _ -> 
	empty
      | TId x -> 
	singleton x
      | TRef t
      | TSource t
      | TSink t ->
	free_typ_ids t
      | TIntersect (t1, t2)
      | TUnion (t1, t2) ->
	union (free_typ_ids t1) (free_typ_ids t2)
      | TArrow (ss, t) 
      | TApp (t, ss) ->
	unions (free_typ_ids t :: (map free_typ_ids ss))
      | TObject o ->
	let sel (_, prop) = match prop_typ prop with
	  | None -> None
	  | Some s -> Some (free_typ_ids s) in
	unions (L.filter_map sel o.fields)
      | TFix (x, _, t)
      | TRec (x, t) ->
	remove x (free_typ_ids t)
      | TForall (x, s, t) ->
	union (free_typ_ids s) (remove x (free_typ_ids t))
      | TLambda (xks, t) ->
	diff (free_typ_ids t) (from_list (map fst2 xks))

  let rec typ_subst x s typ = match typ with
    | TPrim _ -> typ
    | TRegex _ -> typ
    | TId y -> if x = y then s else typ
    | TUnion (t1, t2) -> TUnion (typ_subst x s t1, typ_subst x s t2)
    | TIntersect (t1, t2) ->
      TIntersect (typ_subst x s t1, typ_subst x s t2)
    | TArrow (t2s, t3)  ->
      TArrow (map (typ_subst x s) t2s, typ_subst x s t3)
    | TObject o ->
      TObject { fields = map (second2 (prop_subst x s)) o.fields;
		cached_parent_typ = ref None;
		cached_guard_pat = ref None }
    | TRef t -> TRef (typ_subst x s t)
    | TSource t -> TSource (typ_subst x s t)
    | TSink t -> TSink (typ_subst x s t)
    | TTop -> TTop
    | TBot -> TBot
    | TLambda (yks, t) ->
      let ys = IdSetExt.from_list (map fst2 yks) in
      if IdSet.mem x ys then
	typ
      else begin
	(* TODO: omg this stuff is NOT capture free ... *)
	assert (IdSet.is_empty (IdSet.inter (free_typ_ids s) ys));
	TLambda (yks, typ_subst x s t)
      end
    | TFix (y, k, t) ->
      if x = y then
	typ
      else begin
	assert (not (IdSet.mem y (free_typ_ids s)));
	TFix (y, k, typ_subst x s t)
      end
    | TForall (y, t1, t2) -> 
      if x = y then 
        typ
      else 
        TForall (y, typ_subst x s t1, typ_subst x s t2)
    | TRec (y, t) ->
      if x = y then
	typ
      else begin
	assert (not (IdSet.mem y (free_typ_ids s)));
        TRec (y, typ_subst x s t)
      end
    | TApp (t, ts) -> TApp (typ_subst x s t, List.map (typ_subst x s) ts)

  and prop_subst x s p = match p with
    | PInherited typ -> PInherited (typ_subst x s typ)
    | PPresent typ -> PPresent (typ_subst x s typ)
    | PMaybe typ -> PMaybe (typ_subst x s typ)
    | PAbsent -> PAbsent

  let rec simpl_typ typenv typ = match typ with
    | TPrim _ 
    | TUnion _
    | TIntersect _
    | TRegex _
    | TArrow _
    | TRef _
    | TSource _
    | TSink _
    | TTop _
    | TBot _
    | TLambda _
    | TObject _
    | TId _  
    | TForall _ -> typ
    | TFix (x, k, t) -> simpl_typ typenv (typ_subst x typ t)
    | TRec (x, t) -> simpl_typ typenv (typ_subst x typ t)
    | TApp (t1, ts) -> begin match expose typenv (simpl_typ typenv t1) with
	| TLambda (args, u) -> 
	  simpl_typ typenv 
	    (List.fold_right2 (* well-kinded, no need to check *)
	       (fun (x, k) t2 u -> typ_subst x t2 u)
	       args ts u)
	| _ -> 
	  raise (Invalid_argument "ill-kinded type application in simpl_typ")
    end

  and expose typenv typ = match typ with
    | TId x -> expose typenv (simpl_typ typenv (fst2 (IdMap.find x typenv)))
    | _ -> typ

  (** Decides if two types are syntactically equal. This helps subtyping. *)
  let rec simpl_equiv (typ1 : typ) (typ2 : typ) : bool = match (typ1, typ2) with
    | TTop, TTop
    | TBot, TBot ->
      true
    | TPrim p1, TPrim p2 ->
      p1 = p2
    | TIntersect (s1, s2), TIntersect (t1, t2)
    | TUnion (s1, s2), TUnion (t1, t2) -> 
      simpl_equiv s1 t1 && simpl_equiv s2 t2
    | TSource s, TSource t
    | TSink s, TSink t
    | TRef s, TRef t ->
      simpl_equiv s t
    | TApp (s1, s2s), TApp (t1, t2s) ->
      (* for well-kinded types, for_all2 should not signal an error *)
      simpl_equiv s1 t1 && List.for_all2 simpl_equiv s2s t2s
    | TId x, TId y ->
      x = y
    | TArrow (args1, r1), TArrow (args2, r2) ->
      List.length args1 = List.length args2
      && List.for_all2 simpl_equiv args1 args2
      && simpl_equiv r1 r2
    | TRec (x, s), TRec (y, t) ->
      x = y && simpl_equiv s t
    | TForall (x, s1, s2), TForall (y, t1, t2) ->
      x = y && simpl_equiv s1 t1 && simpl_equiv s2 t2
    | TRegex pat1, TRegex pat2 ->
      P.is_equal pat1 pat2
    | TObject o1, TObject o2 ->
      let flds1 = fields o1 in
      let flds2 = fields o2 in
      List.length flds1 = List.length flds2
      && List.for_all2 simpl_equiv_fld flds1 flds2
    | TFix (x1, k1, t1), TFix (x2, k2, t2) ->
      x1 = x2 && k1 = k2 && simpl_equiv t1 t2
    | TLambda (args1, t1), TLambda (args2, t2) ->
      args1 = args2 && simpl_equiv t1 t2
    | _, _ -> false

  and simpl_equiv_fld (pat1, fld1) (pat2, fld2) = 
    P.is_equal pat1 pat2 &&
      begin match (fld1, fld2) with
	| PPresent t1, PPresent t2
	| PInherited t1, PInherited t2
	| PMaybe t1, PMaybe t2 ->
	  simpl_equiv t1 t2
	| PAbsent, PAbsent -> true
	| _ -> false
      end

  let pat_env (env : typenv) : pat IdMap.t =
    let select_pat_bound (x, (t, _)) = match t with
      | TRegex p -> Some (x, p)
	| _ -> None in
      L.fold_right (fun (x,p) env -> IdMap.add x p env)
	(L.filter_map select_pat_bound (IdMap.bindings env))
	IdMap.empty


  let rec parent_typ' env flds = match flds with
    | [] -> None
    | ((pat, fld) :: flds') -> 
      match P.is_subset (pat_env env) proto_pat pat with
        | true -> begin match fld with
	    | PPresent t -> 
	      begin match expose env (simpl_typ env t) with
		| TPrim Null -> Some (TPrim Null)
		| TSource p
		| TRef p -> Some (expose env (simpl_typ env p))
		| _ -> failwith "invalid parent type"
	      end
	    | _ -> failwith "maybe proto wtf"
        end
        | false -> parent_typ' env flds'

  let rec parent_typ (env : typenv) typ = 
    match expose env (simpl_typ env typ) with
      | TObject ot -> begin match !(ot.cached_parent_typ) with
	  | Some cached ->
	    cached
	  | None ->
	    let computed = parent_typ' env ot.fields in
	    ot.cached_parent_typ := Some computed;
	    computed
      end
      | _ -> raise (Invalid_argument "parent_typ expects TObject")

  let calc_inherit_guard_pat (env : typenv) (t : typ) : pat =
    match t with
      | TObject ot ->
	begin match parent_typ env t with
	  | None
	  | Some (TPrim Null) ->
	    let f (pat, prop) = match prop with
	      | PInherited _
	      | PPresent _ -> Some pat
	      | PMaybe _
	      | PAbsent -> None in
	    L.fold_right P.union (L.filter_map f ot.fields) P.empty
	  | Some (TObject _) ->
	    L.fold_right P.union (L.map fst2 ot.fields) P.empty
	  | Some pt ->
	    raise (Invalid_argument 
		     ("invalid parent type in object type: " ^
			 (string_of_typ pt)))
	end
      | t -> raise (Invalid_argument "expected TObject")

  let inherit_guard_pat env typ = match typ with
    | TObject ot -> begin match !(ot.cached_guard_pat) with
	| None -> let pat = calc_inherit_guard_pat env typ in
		  ot.cached_guard_pat := Some pat;
		  pat
	| Some pat -> pat
    end
    | t -> raise (Invalid_argument ("expected object type, got " ^
				       (string_of_typ t)))

  let maybe_pats flds = 
    let sel (pat, prop) = match prop with
      | PMaybe _ -> Some pat
      | PAbsent -> Some pat
      | _ -> None in
    L.fold_right P.union (L.filter_map sel flds) P.empty


  exception Not_subtype of string

  let mismatched_typ_exn t1 t2 =
    raise (Not_subtype 
	     (sprintf " %s is not a subtype of %s"
		(string_of_typ t1) (string_of_typ t2)))

  let extra_fld_exn pat prop =
    raise (Not_subtype
	     (sprintf "ExtraField - %s : %s\n" (P.pretty pat) 
		(string_of_prop prop)))

  let mismatch_fld_exn (pat1, prop1) (pat2, prop2) =
    raise (Not_subtype
	     ( sprintf "Mismatched - %s : %s and %s : %s\n"
		 (P.pretty pat1) (string_of_prop prop1)
		 (P.pretty pat2) (string_of_prop prop2)))


  let sel_not_absent (p, f) = match prop_typ f with
    | None -> None
    | Some _ -> Some p

  let rec simpl_lookup p (env : typenv) (t : typ) (pat : pat) : typ =
   (* TODO: it's okay to overlap with a maybe, but not a hidden field;
      inherit_guard_pat does not apply *)
    match t with
	| TObject ot -> 
	  let guard_pat = L.fold_right P.union
	    (L.filter_map sel_not_absent ot.fields) P.empty in
	  if P.is_subset (pat_env env) pat guard_pat then
	    let sel (f_pat, f_prop) =
	      if P.is_overlapped f_pat pat then prop_typ f_prop
	      else None in
	    L.fold_right (fun s t -> typ_union env s t)
	      (L.filter_map sel ot.fields)
	      TBot
	  else
      raise (Typ_error (p, "checking for a hidden or absent field"))
	| _ -> raise (Typ_error (p, "object expected"))

  and  inherits (env : typenv) (t : typ) (pat : pat) : typ = 
    let t = expose env (simpl_typ env t) in
    if P.is_subset (pat_env env) pat (inherit_guard_pat env t) then
      begin match t with
	| TObject ot -> 
	  let sel (f_pat, f_prop) =
	    if P.is_overlapped f_pat pat then prop_typ f_prop
	    else None in
	  L.fold_right (fun s t -> typ_union env s t)
	    (L.filter_map sel ot.fields)
	    (match parent_typ env t with
	      | None
	      | Some (TPrim Null) -> TBot
	      | Some parent_typ -> 
		inherits env parent_typ 
		  (P.intersect pat (maybe_pats ot.fields)))
	| _ -> failwith "lookup non-object"
      end
    else
      failwith ("lookup hidden field with " ^ (P.pretty pat) ^ " in "
		   ^ string_of_typ t)

  and subt env (cache : TPSet.t) s t : TPSet.t = 
    if TPSet.mem (s, t) cache then
      cache
    (* workaround for equal: functional value, due to lazy *)
    else if simpl_equiv s t then
      cache
    else
      let subtype = subt env in
      let cache = TPSet.add (s, t) cache in
      let simpl_s = simpl_typ env s in
      let simpl_t = simpl_typ env t in
      match simpl_s, simpl_t with
        | TPrim Int, TPrim Num -> cache
        | TRegex pat1, TRegex pat2 ->
          if P.is_subset (pat_env env) pat1 pat2 then cache 
            else mismatched_typ_exn (TRegex pat1) (TRegex pat2)
        | TIntersect (s1, s2), _ -> 
          begin 
            try subtype cache s1 t
            with Not_subtype _ -> subtype cache s2 t
          end
        | _, TIntersect (t1, t2) ->
            subt env (subt env cache s t1) s t2
        | TUnion (s1, s2), _ -> subt env (subt env cache s1 t) s2 t
        | _, TUnion (t1, t2) ->
          begin 
            try subtype cache s t1
            with Not_subtype _ -> subtype cache s t2
          end
	| TArrow (args1, r1), TArrow (args2, r2) ->
          begin
            try List.fold_left2 subtype cache (r1 :: args2) (r2 :: args1)
            with Invalid_argument _ -> mismatched_typ_exn s t
          end
        | TId x, t -> 
	  subtype cache (fst2 (IdMap.find x env)) t
	| TObject obj1, TObject obj2 ->
	  subtype_object' env cache (fields obj1) (fields obj2)
        | TRef s', TRef t' -> subtype (subtype cache s' t') t' s'
        | TSource s, TSource t -> subtype cache s t
        | TSink s, TSink t -> subtype cache t s
        | TRef s, TSource t -> subtype cache s t
        | TRef s, TSink t -> subtype cache t s
        | TForall (x1, s1, t1), TForall (x2, s2, t2) -> 
	  (* Kernel rule *)
	  (* TODO: ensure s1 = s2 *)
	  let cache' = subt env (subt env cache s1 s2) s2 s1 in
	  let t2 = typ_subst x2 (TId x1) t2 in
          let env' = IdMap.add x1 (s1, KStar) env in
	  subt env' cache' t1 t2
        | _, TTop -> cache
        | TBot, _ -> cache
	| TLambda ([(x, KStar)], s), TLambda ([(y, KStar)], t) ->
	  let env = IdMap.add x (TTop, KStar) env in
	  let env = IdMap.add y (TTop, KStar) env in
	  subt env cache s t
        | _ -> mismatched_typ_exn s t

  (* Check that an "extra" field is inherited *)
  and check_inherited env cache lang other_proto typ =
    subt env cache typ (inherits env other_proto lang)

  and subtype_object' env (cache : TPSet.t) (flds1 : field list)
      (flds2 : field list) : TPSet.t =
    let rec subtype_field env cache ((pat1, fld1) : field) 
        ((pat2, fld2) : field) : TPSet.t = 
      match (fld1, fld2) with
        | (PAbsent, PAbsent) -> cache
        | (PAbsent, PMaybe _) -> cache
        | (PPresent t1, PPresent t2) -> subt env cache t1 t2
        | (PPresent t1, PMaybe t2) -> subt env cache t1 t2
        | (PMaybe t1, PMaybe t2) -> subt env cache t1 t2
        | (_, PInherited _) -> cache
        | _ -> mismatch_fld_exn (pat1, fld1) (pat2, fld2) in
    let rec check_prop (((m_j : pat), (g_j : prop)) as p2)
        ((p2s : field list), (fs : field list), (cache : TPSet.t)) = 
      match fs with
        | [] -> (p2::p2s, fs, cache)
        | (l_i, f_i)::rest ->
          match P.is_overlapped l_i m_j with
            | true ->
              let cache' = subtype_field env cache (l_i, f_i) (m_j, g_j) in
            (* Remove the stuff in l_i (the lhs), for the next iteration *)
              let (pat'', _) as p2' = ((P.subtract m_j l_i), g_j) in
            (* Get the result, and re-build the rhs list, subtracting the
               relevant part of overlap*)
              let p2s'', fs1', cache'' = check_prop p2' (p2s, rest, cache') in
              p2s'', (P.subtract l_i m_j, f_i)::fs1', cache'
            | false ->
              let p2s', fs1', cache' = check_prop p2 (p2s, rest, cache) in
              p2s', (P.subtract l_i m_j, f_i)::fs1', cache' in
    let (flds2', flds1', cache') = 
      List.fold_right check_prop flds2 ([], flds1, cache) in
    let cache' = List.fold_right (fun (pat, prop) cache -> match prop with
      | PInherited typ -> 
	(* TODO BAD PERFORMANCE *)
        check_inherited env cache pat (TObject (mk_obj_typ flds1)) typ
      | _ -> cache)
      flds2 cache in
    try
      let (pat, fld) =
	List.find (fun (pat, _) -> not (P.is_empty pat)) flds2' in
      match fld with 
        | PInherited _ -> cache'
        | _ -> extra_fld_exn pat fld
    with Not_found -> cache'


  and subtypes env ss ts = 
    try 
      let _ = List.fold_left2 (subt env) TPSet.empty ss ts in
      true
    with 
      | Invalid_argument _ -> false (* unequal lengths *)
      | Not_subtype _ -> false

  and subtype env s t = 
    try
      let _ = subt env TPSet.empty s t in
      true
    with Not_subtype _ -> false
      | Not_found -> failwith "not found in subtype!!!"

  and typ_union cs s t = match subtype cs s t, subtype cs t s with
      true, true -> s (* t = s *)
    | true, false -> t (* s <: t *)
    | false, true -> s (* t <: s *)
    | false, false -> TUnion (s, t)

  and typ_intersect cs s t = match subtype cs s t, subtype cs t s with
    | true, true -> s
    | true, false -> s (* s <: t *)
    | false, true -> t
    | false, false -> TIntersect (s, t)

  let subtype env s t = 
    try
      let _ = subt env TPSet.empty s t in
      true
    with Not_subtype _ -> false

  let assert_subtyp  env p s t = 
(*      let _ = subt env TPSet.empty s t in
      () *)
    try 
      let _ = subt env TPSet.empty s t in
      ()
    with
      | Not_subtype txt -> raise (Typ_error (p, txt ^ "\nin "
	^ (string_of_typ s) ^ "\n\nand\n\n" ^ (string_of_typ t)))


end

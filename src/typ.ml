open Prelude
open Sig

module L = ListExt

module Make (P : PAT) : (TYP with type pat = P.t) = struct


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

  let proto_str = "__proto__"
    
  let proto_pat = P.singleton proto_str

      
  let mk_obj_typ (fs: field list) : obj_typ = 
    { fields = fs;
      cached_parent_typ = ref None;
      cached_guard_pat = ref None }

  let fields ot = ot.fields

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
  (* TODO: omg this stuff is NOT capture free ... *)
    | TLambda (args, t) ->
      TLambda (args, typ_subst x s t)
    | TFix (y, k, t) ->
      TFix (y, k, typ_subst x s t)
    | TForall (y, t1, t2) -> 
      if x = y then 
        typ
      else 
        TForall (y, typ_subst x s t1, typ_subst x s t2)
    | TRec (y, t) ->
      if x = y then
        failwith "TODO: capture free substitution"
      else 
        TRec (y, typ_subst x s t)
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

  let rec parent_typ' env flds = match flds with
    | [] -> None
    | ((pat, fld) :: flds') -> match P.is_member proto_str pat with
        | true -> begin match fld with
	    | PPresent t -> 
	      begin match expose env t with
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


  let prop_typ prop = match prop with
    | PInherited t
    | PPresent t
    | PMaybe t -> Some t
    | PAbsent -> None


  let maybe_pats flds = 
    let sel (pat, prop) = match prop with
      | PMaybe _ -> Some pat
      | _ -> None in
    L.fold_right P.union (L.filter_map sel flds) P.empty

  let rec inherits (env : typenv) (t : typ) (pat : pat) : typ = 
    let t = expose env (simpl_typ env t) in
    if P.is_subset pat (inherit_guard_pat env t) then
      begin match t with
	| TObject ot -> 
	  let sel (f_pat, f_prop) =
	    if P.is_overlapped f_pat pat then prop_typ f_prop
	    else None in
	  L.fold_right (fun s t -> TUnion (s, t))
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
      failwith "lookup hidden field"

end

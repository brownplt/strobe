open Prelude
open Sig

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


  let rec parent_typ' typenv flds = match flds with
    | [] -> None
    | ((pat, fld) :: flds') -> match P.is_member proto_str pat with
        | true -> begin match fld with
	    | PPresent t -> Some (simpl_typ typenv t)
	    | _ -> failwith "maybe proto wtf"
        end
        | false -> parent_typ' typenv flds'

  let rec parent_typ (typenv : typenv) typ = match typ with
    | TObject ot -> begin match !(ot.cached_parent_typ) with
	| Some cached -> cached
	| None ->
	  let computed = parent_typ' typenv ot.fields in
	  ot.cached_parent_typ := Some computed;
	  computed
    end
    | _ -> raise (Invalid_argument "parent_typ expects TObject")

  let inherit_guard_pat (t : typ) : pat = failwith "nyi"

end

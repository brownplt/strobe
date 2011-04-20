open Prelude
open Typedjs_syntax

exception Not_wf_typ of string

module P = Sb_strPat

let desugar_typ = Sb_desugar.desugar_typ

(* Necessary for equi-recursive subtyping. *)
module TypPair = struct
  type t = typ * typ
  let compare = Pervasives.compare
end

module TPSet = Set.Make (TypPair)
module TPSetExt = SetExt.Make (TPSet)

module List = struct
  include List

  let rec tails (lst : 'a list) : 'a list list = match lst with
    | [] -> [ [] ]
    | _ :: lst' -> lst :: (tails lst')

  let iter_pairs (f : 'a -> 'a -> unit) (lst : 'a list) : unit =
    let g lst = match lst with
      | x :: rest -> iter (f x) rest
      | _ -> () in
    iter g (tails lst)

  let rec map2_noerr (f : 'a -> 'b -> 'c) (xs : 'a list) (ys : 'b list) =
    match (xs, ys) with
      | [], _ -> []
      | _, [] -> []
      | x :: xs', y :: ys' -> (f x y) :: map2_noerr f xs' ys'

  let rec filter_map (f : 'a -> 'b option) (xs : 'a list) : 'b list =
    match xs with
      | [] -> []
      | x :: xs' -> match f x with
	  | None -> filter_map f xs'
	  | Some y -> y :: (filter_map f xs')
	    
end

let rec typ_subst x s typ = match typ with
  | TPrim _ -> typ
  | TRegex _ -> typ
  | TId y -> if x = y then s else typ
  | TUnion (t1, t2) -> TUnion (typ_subst x s t1, typ_subst x s t2)
  | TIntersect (t1, t2) ->
      TIntersect (typ_subst x s t1, typ_subst x s t2)
  | TArrow (t2s, t3)  ->
      TArrow (map (typ_subst x s) t2s, typ_subst x s t3)
  | TObject (flds, proto) -> 
      TObject (map (second2 (prop_subst x s)) flds, typ_subst x s proto)
  | TSimpleObject flds ->
    TSimpleObject (map (second2 (prop_subst x s)) flds)
  | TRef t -> TRef (typ_subst x s t)
  | TSource t -> TSource (typ_subst x s t)
  | TSink t -> TSink (typ_subst x s t)
  | TTop -> TTop
  | TBot -> TBot
  | TField -> TField
  (* omg this stuff is NOT capture free ... *)
  | TLambda (y, k, t) ->
    TLambda (y, k, typ_subst x s t)
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
  | TApp (t1, t2) -> TApp (typ_subst x s t1, typ_subst x s t2)

and prop_subst x s p = match p with
  | PPresent typ -> PPresent (typ_subst x s typ)
  | PMaybe typ -> PMaybe (typ_subst x s typ)
  | PAbsent -> PAbsent


module Env = struct

  type class_info = {
    fields : typ IdMap.t;
    sup : constr option
  }

  type env = {
    id_typs : typ IdMap.t; (* type of term identifiers *)
    lbl_typs : typ IdMap.t; (* types of labels *)
    typ_ids: typ IdMap.t; (* bounded type variables *)
  }


  let empty_env = { 
    id_typs = IdMap.empty;
    lbl_typs = IdMap.empty;
    typ_ids = IdMap.empty;
  }

  let bind_id x t env  = { env with id_typs = IdMap.add x t env.id_typs }

  let bind_lbl x t env = { env with lbl_typs = IdMap.add x t env.lbl_typs }

  let bind_typ_id x t env = { env with typ_ids = IdMap.add x t env.typ_ids }

  let lookup_id x env = IdMap.find x env.id_typs

  let lookup_lbl x env = IdMap.find x env.lbl_typs

  let id_env env = env.id_typs

  let clear_labels env = { env with lbl_typs = 
      try 
        IdMap.add "%return" (IdMap.find "%return" env.lbl_typs) IdMap.empty
      with Not_found -> IdMap.empty }

  let dom env = IdSetExt.from_list (IdMapExt.keys env.id_typs)

  exception Not_subtype

  let rec bind_typ env typ : env * typ = match typ with
    | TForall (x, s, t) -> bind_typ (bind_typ_id x s env) (typ_subst x s t)
    | typ -> (env, typ)


  let rec simpl_typ env typ = match typ with
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
    | TField _
    | TLambda _
    | TSimpleObject _
    | TForall _ -> typ
    | TRec (x, t) -> simpl_typ env (typ_subst x typ t)
    | TId x -> simpl_typ env (IdMap.find x env.typ_ids)
    | TApp (t1, t2) -> begin match simpl_typ env t1 with
	| TLambda (x, KStar, u) -> 
	  simpl_typ env (typ_subst x t2 u)
	| _ -> raise
	  (Not_wf_typ (sprintf "%s in type-application position"
			 (string_of_typ t1)))
    end
    | TObject (fs, proto) ->
      TObject (fs, simpl_typ env proto)


  let get_fld idx (pat, prop) = 
    let pat' = P.intersect pat idx in
    if P.is_empty pat' then
      None
    else 
      Some (pat', prop)

  let rec flatten_obj_typ_helper env (idx : pat) (typ : typ) =
    match simpl_typ env typ with
      | TObject (flds, TSource proto)
      | TObject (flds, TRef proto) ->
	let proto_idx = 
	  P.subtract idx (* idx - present patterns *)
	  (fold_left P.union P.empty 
	     (map fst2 (List.filter Typ.is_present flds))) in
	(List.filter_map (get_fld idx) flds) @ 
	  (flatten_obj_typ_helper env proto_idx proto)
      | TObject (flds, TPrim Null) ->
	List.filter_map (get_fld idx) flds
      | _ -> failwith (sprintf "expected object type")

  let flatten_obj_typ env obj_typ = 
    TSimpleObject (flatten_obj_typ_helper env P.all obj_typ)

  let rec subt env (cache : TPSet.t) s t : TPSet.t= 
    if TPSet.mem (s, t) cache then
      cache
    (* workaround for equal: functional value, due to lazy *)
    else if Sb_kinding.simpl_equiv s t then
      cache
    else
      let subtype = subt env in
      let cache = TPSet.add (s, t) cache in
      match simpl_typ env s, simpl_typ env t with
        | TPrim Int, TPrim Num -> cache
        | TRegex pat1, TRegex pat2 ->
          if P.contains pat1 pat2 then cache 
            else raise Not_subtype
        | TId x, TId y -> if x = y then cache else raise Not_subtype
        | TId x, t -> begin try
          let s = IdMap.find x env.typ_ids in (* S-TVar *)
            subtype cache s t (* S-Trans *)
          with Not_found -> failwith (sprintf "failed looking up %s" x) end
        | t, TId y -> begin try
          let s = IdMap.find y env.typ_ids in (* S-TVar *)
            subtype cache t s (* S-Trans *)
          with Not_found -> failwith (sprintf "failed looking up %s" y) end
        | TIntersect (s1, s2), _ -> 
          begin 
            try subtype cache s1 t
            with Not_subtype -> subtype cache s2 t
          end
        | _, TIntersect (t1, t2) ->
            subt env (subt env cache s t1) s t2
        | TUnion (s1, s2), _ -> subt env (subt env cache s1 t) s2 t
        | _, TUnion (t1, t2) ->
          begin 
            try subtype cache s t1
            with Not_subtype -> subtype cache s t2
          end
	| TArrow (args1, r1), TArrow (args2, r2) ->
          begin
            try List.fold_left2 subtype cache (r1 :: args2) (r2 :: args1)
            with Invalid_argument _ -> raise Not_subtype (* unequal lengths *)
          end
	| TSimpleObject flds1, TSimpleObject flds2 ->
	  let f cache fld1 =
	    fold_left (fun cache fld2 -> subtype_field env cache fld1 fld2)
	      cache flds2 in
	  fold_left f cache flds1
	| TObject (flds, proto) as lhs, (TSimpleObject _ as rhs) ->
	  subtype cache (flatten_obj_typ env lhs) rhs
	| TObject (fs1, p1), TObject (fs2, p2) -> 
            subtype (subtype_object' env cache fs1 fs2) p1 p2
        | TRef s', TRef t' -> subtype (subtype cache s' t') t' s'
        | TSource s, TSource t -> subtype cache s t
        | TSink s, TSink t -> subtype cache t s
        | TRef s, TSource t -> subtype cache s t
        | TRef s, TSink t -> subtype cache t s
        | TForall (x1, s1, t1), TForall (x2, s2, t2) -> 
	  (* Kernel rule *)
	  printf "Maybe undecidable...\n%!";
(*	  let cache = subt env cache s1 s2 in
	  let cache = subt env cache s2 s1 in 
	  let env = bind_typ_id x1 s1 env in
	  let env = bind_typ_id x2 s2 env in *)

	  
            let (env', typ') = bind_typ env s in
            let (env'', typ'') = bind_typ env t in
              subt env cache typ' typ''
        | _, TTop -> cache
        | TBot, _ -> cache
	| TLambda (x, KStar, s), TLambda (y, KStar, t) ->
	  let env = bind_typ_id x TTop env in
	  let env = bind_typ_id y TTop env in
	  subt env cache s t
        | _ -> raise Not_subtype

  and subtype_field env cache ((pat1, fld1) : field * prop) 
      ((pat2, fld2) : field * prop) : TPSet.t = 
    if P.is_empty (P.intersect pat1 pat2) then
      cache
    else
      match (fld1, fld2) with
	| (PAbsent, PAbsent) -> cache
	| (PAbsent, PMaybe _) -> cache
	| (PPresent t1, PPresent t2) -> subt env cache t1 t2
	| (PPresent t1, PMaybe t2) -> subt env cache t1 t2
	| (PMaybe t1, PMaybe t2) -> subt env cache t1 t2
	| _ -> raise Not_subtype

  and subtype_object' env (cache : TPSet.t)
    (flds1 : (field * prop) list)
    (flds2 : (field * prop) list) : TPSet.t
      =
    (* TODO: check for containment *)
    let g cache' fld1 = 
      fold_left (fun cache fld2 -> subtype_field env cache fld1 fld2)
	cache' flds2 in
    fold_left g cache flds1


  (* subtype_prop encodes this lattice:

        PMaybe T
       /        \
    PPresent  PAbsent
  *)
  and subtype_prop env cache p1 p2 = match p1, p2 with
    | PPresent s, PPresent t
    | PPresent s, PMaybe t
    | PMaybe s, PMaybe t -> subt env cache s t
    | PAbsent, PAbsent
    | PAbsent, PMaybe _ -> cache
    | _ -> raise Not_subtype
    
  and subtypes env ss ts = 
    try 
      let _ = List.fold_left2 (subt env) TPSet.empty ss ts in
      true
    with 
      | Invalid_argument _ -> false (* unequal lengths *)
      | Not_subtype -> false

  and subtype env s t = 
    try
      let _ = subt env TPSet.empty s t in
      true
    with Not_subtype -> false
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

  let basic_static env (typ : typ) (rt : RT.t) : typ = match rt with
    | RT.Num -> typ_union env (TPrim Num) typ
    | RT.Re _ -> typ_union env (TRegex any_fld) typ
    | RT.Bool -> typ_union env typ_bool typ
    | RT.Function -> typ_union env TField typ
    | RT.Object -> typ_union env TField typ
    | RT.Undefined -> typ_union env (TPrim Undef) typ

  let basic_static2 env (typ : typ) (rt : RT.t) : typ = match rt with
    | RT.Num -> typ_union env (TPrim Num) typ
    | RT.Re _ -> typ_union env (TRegex any_fld) typ
    | RT.Bool -> typ_union env typ_bool typ
    | RT.Function -> typ_union env (TObject ([], TId "Function")) typ 
    | RT.Object -> typ_union env (TObject ([], TId "Object")) typ
    | RT.Undefined -> typ_union env (TPrim Undef) typ

  let rtany = 
    RTSetExt.from_list
    [ RT.Num; RT.Re any_fld; RT.Bool; RT.Function; RT.Object; RT.Undefined ]

  let is_re rt = match rt with RT.Re _ -> true | _ -> false

  let match_re rts = if RTSet.cardinal rts = 1 then
      match RTSet.choose rts with
        | RT.Re re -> Some re
        | _ -> None
    else None

  let rec static cs (rt : RTSet.t) (typ : typ) : typ = match typ with
    | TBot -> TBot (* might change if we allow arbitrary casts *)
    | TArrow _ -> if RTSet.mem RT.Function rt then typ else TBot
    | TRegex _ -> if RTSet.exists is_re rt then 
        match match_re rt with
          | Some re -> TRegex re
          | None -> typ
      else TBot
    | TPrim (Num) 
    | TPrim (Int) -> if RTSet.mem RT.Num rt then typ else TBot
    | TPrim (True)
    | TPrim (False) -> if RTSet.mem RT.Bool rt then typ else TBot
    | TPrim (Null) -> if RTSet.mem RT.Object rt then typ else TBot
    | TPrim (Undef) -> 
        if RTSet.mem RT.Undefined rt then typ else TBot
          (* any other app will be an object from a constructor *)
    | TRef (TSimpleObject _)
    | TRef (TObject _) -> if RTSet.mem RT.Object rt then typ else TBot
    | TSimpleObject _
    | TObject _ -> failwith "Static got a functional object"
    | TRef t -> TRef t
    | TSource t -> TSource t
    | TSink t -> TSink t
    | TUnion (s, t) -> typ_union cs (static cs rt s) (static cs rt t)
    | TIntersect (s, t) -> typ_intersect cs (static cs rt s) (static cs rt t)
    | TForall _ -> typ
    | TField -> List.fold_left (basic_static cs) TBot (RTSetExt.to_list rt)
    | TTop -> 
        if RTSet.equal rt rtany then
          TTop
        else (* TODO: no arrow type that is the supertype of all arrows *)
          List.fold_left (basic_static2 cs) TBot (RTSetExt.to_list rt)
    | TId _ -> typ
    | TRec (x, t) -> let t' = TRec (x, static cs rt t) in
                     (match t' with TRec (_, TBot) -> TBot | typ -> typ)
    | TLambda _ -> failwith "TLambda in static"
    | TApp _ -> typ

  let rec set_global_object env cname =
    let ci = IdMap.find cname env.typ_ids in
    match ci with
      | TRef (TObject (fs, _)) ->
        let add_field env ((x : field), (p : prop)) = 
	  begin match P.singleton_string x, p with
            | (Some s, PPresent t) -> bind_id s (TRef t) env
            | _, PAbsent -> env
            | _ -> raise (Not_wf_typ (cname ^ " field was a regex in global"))
        end in
        List.fold_left add_field env fs
      | _ -> 
        raise (Not_wf_typ (cname ^ " global must be an object"))

end

open Lexing

let parse_env (cin : in_channel) (name : string) : env_decl list =
  let lexbuf = Lexing.from_channel cin in
    try
      lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with 
                                      Lexing.pos_fname = name };
      Typedjs_parser.env Typedjs_lexer.token lexbuf
    with
      | Failure "lexing: empty token" ->
          failwith (sprintf "error lexing environment at %s"
                      (string_of_position 
                         (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))
      | Typedjs_parser.Error ->
          failwith (sprintf "error parsing environment at %s"
                      (string_of_position 
                         (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))

let extend_global_env env lst =
  let add env decl = match decl with
    | EnvBind (p, x, typ) ->
      if IdMap.mem x env.Env.id_typs then
        raise (Not_wf_typ (x ^ " is already bound in the environment"))
      else
        Env.bind_id x (desugar_typ p typ) env
    | EnvType (p, x, t) ->
      if IdMap.mem x env.Env.typ_ids then
	raise (Not_wf_typ (sprintf "the type %s is already defined" x))
      else
	{ env with 
	  Env.typ_ids = IdMap.add x (desugar_typ p t) env.Env.typ_ids }
  in List.fold_left add env lst

(*

let df_func_of_typ syns (t : typ) : L.av list -> L.av = match t with
  | TArrow (_, r_typ) ->
      let r_av = L.ASet (L.rt_of_typ syns r_typ) in
        (fun _ -> r_av)
  | TForall (x, r_typ, TArrow (_, TId y)) when x = y ->
      let r_av = L.ASet (L.rt_of_typ syns r_typ) in
        (fun _ -> r_av)
  | _ -> (fun _ -> L.any)

let cf_env_of_tc_env tc_env = 
  let fn x typ cf_env = L.bind x (L.runtime tc_env.Env.typ_syns typ) cf_env in
    IdMap.fold fn (Env.id_env tc_env) L.empty_env

let operator_env_of_tc_env tc_env =
  let fn x t env = IdMap.add x (df_func_of_typ (Env.syns tc_env) t) env in
    IdMap.fold fn (Env.id_env tc_env) IdMap.empty
*)

let simpl_typ = Env.simpl_typ

let apply_subst subst typ = IdMap.fold typ_subst subst typ

let assoc_merge = IdMap.merge (fun x opt_s opt_t -> match opt_s, opt_t with
  | Some (TId y), Some (TId z) -> 
    if x = y then opt_t else opt_s
  | Some (TId _), Some t 
  | Some t, Some (TId _) -> Some t
  | Some t, _
  | _, Some t ->
    Some t
  | None, None -> None)

let rec typ_assoc (env : Env.env) (typ1 : typ) (typ2 : typ) = 
  match (typ1, typ2) with
    | TId x, _ -> IdMap.singleton x typ2
    | TApp (s1, s2), TApp (t1, t2)
    | TIntersect (s1, s2), TIntersect (t1, t2)
    | TUnion (s1, s2), TUnion (t1, t2) -> 
      assoc_merge (typ_assoc env s1 t1) (typ_assoc env s2 t2)

    | TApp (s1, s2), t
    | t, TApp (s1, s2) ->
      typ_assoc env (simpl_typ env (TApp (s1, s2))) t

    | TObject (flds1, proto1), TObject (flds2, proto2) ->
      List.fold_left assoc_merge
	(typ_assoc env proto1 proto2)
	(List.map2_noerr (fld_assoc env) flds1 flds2)
    | TSource s, TSource t
    | TSink s, TSink t
    | TRef s, TRef t ->
      typ_assoc env s t
    | TArrow (args1, r1), TArrow (args2, r2) ->
      List.fold_left assoc_merge
	(typ_assoc env r1 r2)
	(List.map2_noerr (typ_assoc env) args1 args2)
    | TRec (x, s), TRec (y, t) ->
      (* could do better here, renaming*)
      typ_assoc env s t
    | TForall (x, s1, s2), TForall (y, t1, t2) ->
      (* also here *)
      assoc_merge (typ_assoc env s1 t1) (typ_assoc env s2 t2)
    | _ -> IdMap.empty

and fld_assoc env (_, fld1) (_, fld2) = match (fld1, fld2) with
  | PPresent s, PPresent t
  | PMaybe s, PMaybe t ->
    typ_assoc env s t
  | _ -> IdMap.empty

let rec fields_helper env flds idx_pat =  match flds with
  | [] -> (TBot, idx_pat)
  | (pat, fld) :: flds' ->
    if P.is_overlapped pat idx_pat then
      match fld with
	| PMaybe t ->
	  let (fld_typ', rest_pat') = fields_helper env flds' idx_pat in
	  (Env.typ_union env t fld_typ', rest_pat')
	| PPresent t -> 
	  let (fld_typ', rest_pat') =
	    fields_helper env flds' (P.subtract idx_pat pat) in
	  (Env.typ_union env t fld_typ', rest_pat')
	| PAbsent -> fields_helper env flds' idx_pat
    else
      fields_helper env flds' idx_pat

let rec fields p env obj_typ idx_pat = match simpl_typ env obj_typ with
  | TSimpleObject flds ->
    let (fld_typ, rest_pat) = fields_helper env flds idx_pat in
    fld_typ
  | TObject (flds, TPrim Null) -> 
    let (fld_typ, rest_pat) = fields_helper env flds idx_pat in
    fld_typ
  | TObject (flds, TRef proto)
  | TObject (flds, TSource proto) ->
    let (fld_typ, rest_pat) = fields_helper env flds idx_pat in
    Env.typ_union env fld_typ (fields p env proto rest_pat)
  | TObject (_, proto) ->
    raise 
      (Typ_error
	 (p, sprintf "cannot index object; prototype has type %s" 
	   (string_of_typ proto)))
  | obj_typ -> 
    failwith (sprintf "fields expected object, received %s"
		(string_of_typ obj_typ))

let typid_env env = env.Env.typ_ids

open Prelude
open Typedjs_syntax

exception Not_wf_typ of string

module P = Sb_strPat

let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)

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

(* Pair that were mismatched *)
type subtype_exn =
  | ExtraFld of pat * prop
  | MismatchTyp of typ * typ
  | MismatchFld of (pat * prop) * (pat * prop)

exception Not_subtype of subtype_exn

let rec typ_subst x s typ = match typ with
  | TPrim _ -> typ
  | TRegex _ -> typ
  | TId y -> if x = y then s else typ
  | TUnion (t1, t2) -> TUnion (typ_subst x s t1, typ_subst x s t2)
  | TIntersect (t1, t2) ->
      TIntersect (typ_subst x s t1, typ_subst x s t2)
  | TArrow (t2s, t3)  ->
      TArrow (map (typ_subst x s) t2s, typ_subst x s t3)
  | TObject flds ->
      TObject (map (second2 (prop_subst x s)) flds)
  | TRef t -> TRef (typ_subst x s t)
  | TSource t -> TSource (typ_subst x s t)
  | TSink t -> TSink (typ_subst x s t)
  | TTop -> TTop
  | TBot -> TBot
  (* omg this stuff is NOT capture free ... *)
  | TLambda (y, k, t) ->
    TLambda (y, k, typ_subst x s t)
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
    typ_ids: (typ * kind) IdMap.t; (* bounded type variables *)
  }


  let empty_env = { 
    id_typs = IdMap.empty;
    lbl_typs = IdMap.empty;
    typ_ids = IdMap.empty;
  }

  let kind_check env typ =
    Sb_kinding.kind_check
      (IdMap.map (fun (_, k) -> k) env.typ_ids)
      typ

  let bind_id x t env  = { env with id_typs = IdMap.add x t env.id_typs }

  let bind_lbl x t env = { env with lbl_typs = IdMap.add x t env.lbl_typs }

  let bind_typ_id x t env = 
    let k = kind_check env t in
    { env with 
      typ_ids = IdMap.add x (t, k) env.typ_ids }

  let lookup_id x env = IdMap.find x env.id_typs

  let lookup_lbl x env = IdMap.find x env.lbl_typs

  let id_env env = env.id_typs

  let clear_labels env = { env with lbl_typs = 
      try 
        IdMap.add "%return" (IdMap.find "%return" env.lbl_typs) IdMap.empty
      with Not_found -> IdMap.empty }

  let dom env = IdSetExt.from_list (IdMapExt.keys env.id_typs)

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
    | TLambda _
    | TObject _
    | TForall _ -> typ
    | TFix (x, k, t) -> simpl_typ env (typ_subst x typ t)
    | TRec (x, t) -> simpl_typ env (typ_subst x typ t)
    | TId x -> simpl_typ env (fst2 (IdMap.find x env.typ_ids))
    | TApp (t1, t2) -> begin match simpl_typ env t1 with
	| TLambda (x, KStar, u) -> 
	  simpl_typ env (typ_subst x t2 u)
	| _ -> raise
	  (Not_wf_typ (sprintf "%s in type-application position"
			 (string_of_typ t1)))
    end


  let get_fld idx (pat, prop) = 
    let pat' = P.intersect pat idx in
    if P.is_empty pat' then
      None
    else 
      Some (pat', prop)



  let rec subt env (cache : TPSet.t) s t : TPSet.t = 
    if TPSet.mem (s, t) cache then
      cache
    (* workaround for equal: functional value, due to lazy *)
    else if Sb_kinding.simpl_equiv s t then
      cache
    else
      let subtype = subt env in
      let cache = TPSet.add (s, t) cache in
      let simpl_s = simpl_typ env s in
      let simpl_t = simpl_typ env t in
      match simpl_s, simpl_t with
        | TPrim Int, TPrim Num -> cache
        | TRegex pat1, TRegex pat2 ->
          if P.contains pat1 pat2 then cache 
            else raise (Not_subtype (MismatchTyp (TRegex pat1, TRegex pat2)))

      (* TId cases should be handled by simpl_typ 
        | TId x, TId y -> 
	  if x = y then 
	    cache 
	  else 
	    raise (Not_subtype (MismatchTyp (TId x, TId y)))
        | TId x, t -> begin try
          let s = IdMap.find x env.typ_ids in (* S-TVar *)
            subtype cache s t (* S-Trans *)
          with Not_found -> failwith (sprintf "failed looking up %s" x) end
        | t, TId y -> begin try
          let s = IdMap.find y env.typ_ids in (* S-TVar *)
            subtype cache t s (* S-Trans *)
          with Not_found -> failwith (sprintf "failed looking up %s" y) end
      *)
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
            with Invalid_argument _ -> raise (Not_subtype (MismatchTyp (s, t)))
          end
	| TObject flds1, TObject flds2 ->
	  subtype_object' env cache flds1 flds2
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
        | _ -> raise (Not_subtype (MismatchTyp (s, t)))

  and get_prototype_typ env flds = match flds with
    | [] -> None
    | ((pat, fld) :: flds') -> match P.is_member proto_str pat with
        | true -> begin match fld with
	    | PPresent t -> Some (simpl_typ env t)
	    | _ -> failwith "maybe proto wtf"
        end
        | false -> get_prototype_typ env flds'


  and fields_helper env flds idx_pat idx_for_proto =  match flds with
    | [] -> (TBot, idx_pat, idx_for_proto)
    | (pat, fld) :: flds' ->
      if P.is_overlapped pat idx_pat then
        match fld with
	  | PMaybe t ->
	    let (fld_typ', all_pat, rest_pat') = fields_helper env flds' idx_pat idx_for_proto in
	    (typ_union env t fld_typ', P.subtract all_pat pat, rest_pat')
          | PInherited t
	  | PPresent t -> 
	    let (fld_typ', all_pat, rest_pat') =
	      fields_helper env flds' idx_pat (P.subtract idx_for_proto pat) in
	    (typ_union env t fld_typ', P.subtract all_pat pat, rest_pat')
	  | PAbsent -> 
            let (fld_typ, all_pat, rest_pat) = fields_helper env flds' idx_pat idx_for_proto in
            (fld_typ, P.subtract all_pat pat, rest_pat)
      else
        fields_helper env flds' idx_pat idx_pat

  and fields p env obj_typ idx_pat = match simpl_typ env obj_typ with
    | TObject flds -> 
      let (fld_typ, all_pat, rest_pat) = fields_helper env flds idx_pat idx_pat in
      if not (P.is_empty all_pat) then 
        raise (Typ_error
                 (p, sprintf "Something left when performing fields: %s"
                   (P.pretty all_pat)))
      else
        begin match get_prototype_typ env flds with
          | None -> 
	    if P.is_empty rest_pat then
	      fld_typ
	    else
	      raise (Typ_error
		       (p, sprintf "no type for __proto__ in:\n %s\nBut, index is \
                                \n %s\n" (string_of_typ obj_typ)
		         (P.pretty rest_pat)))
          | Some (TPrim Null) ->
	    typ_union env fld_typ TBot (** might signal an error *)
          | Some (TRef proto_typ)
          | Some (TSource proto_typ) ->
	    typ_union env fld_typ (fields p env proto_typ rest_pat)
          | Some typ ->
	    raise (Typ_error
		     (p, sprintf "prototype has type\n  %s\nin object of type\n  %s"
		       (string_of_typ typ) (string_of_typ obj_typ)))
        end
    | obj_typ ->
      raise (Typ_error
	       (p, sprintf "expected object, received %s" 
	         (string_of_typ obj_typ)))


  (* Check that an "extra" field is inherited *)
  and check_inherited env cache lang other_proto typ =
    try
      let ftyp = fields dummy_pos env other_proto lang in
      subt env cache typ ftyp 
    with Typ_error _ ->
      raise (Not_subtype (ExtraFld (lang, PInherited typ)))

  and subtype_object' env (cache : TPSet.t)
    (flds1 : (field * prop) list)
    (flds2 : (field * prop) list) : TPSet.t
      =
    let rec subtype_field env cache ((pat1, fld1) : field * prop) 
        ((pat2, fld2) : field * prop) : TPSet.t = 
      match (fld1, fld2) with
        | (PAbsent, PAbsent) -> cache
        | (PAbsent, PMaybe _) -> cache
        | (PPresent t1, PPresent t2) -> subt env cache t1 t2
        | (PPresent t1, PMaybe t2) -> subt env cache t1 t2
        | (PMaybe t1, PMaybe t2) -> subt env cache t1 t2
        | (_, PInherited _) -> cache
        | _ -> raise (Not_subtype (MismatchFld ((pat1, fld1), (pat2, fld2)))) in

    let rec check_prop (((m_j : field), (g_j : prop)) as p2)
        ((p2s : (field * prop) list), (fs : (field * prop) list), (cache : TPSet.t)) = 
      match fs with
        | [] -> (p2::p2s, fs, cache)
        | ((l_i, f_i) as p1)::rest ->
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
        check_inherited env cache pat (TObject flds1) typ
      | _ -> cache)
      flds2 cache in
    try
      let (pat, fld) = List.find (fun (pat, _) -> not (P.is_empty pat)) flds2' in
      raise (Not_subtype (ExtraFld (pat, fld)))
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
    with
      | Not_subtype (ExtraFld (pat, prop)) -> 
(*        printf "ExtraField - %s : %s\n" (P.pretty pat) (string_of_prop prop); *)
        false
      | Not_subtype (MismatchFld ((pat1, prop1), (pat2, prop2)) ) ->
(*        printf "Mismatched - %s : %s and %s : %s\n"
          (P.pretty pat1) (string_of_prop prop1)
          (P.pretty pat2) (string_of_prop prop2); *)
        false
      | Not_subtype (MismatchTyp (s, t)) ->
(*        printf "MismatchTyp - %s <: %s\n" 
          (string_of_typ s) (string_of_typ t); *)
        false
      | Not_found -> failwith "not found in subtype!!!"

  let assert_subtyp env s t = 
    let _ = subt env TPSet.empty s t in
    ()


  let simpl_static env (typ : typ) (rt : RT.t) : typ = match rt with
    | RT.Num -> typ_union env (TPrim Num) typ
    | RT.Re _ -> typ_union env (TRegex any_fld) typ
    | RT.Bool -> typ_union env typ_bool typ
    | RT.Function ->
      typ_union env (TObject [(proto_pat, PPresent (TId "Function"))]) typ 
    | RT.Object -> 
      typ_union env (TObject [proto_pat, PPresent (TId "Object")]) typ
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
    | TRef (TObject _) -> if RTSet.mem RT.Object rt then typ else TBot
    | TObject _ -> failwith "Static got a functional object"
    | TRef t -> TRef t
    | TSource t -> TSource t
    | TSink t -> TSink t
    | TUnion (s, t) -> typ_union cs (static cs rt s) (static cs rt t)
    | TIntersect (s, t) -> typ_intersect cs (static cs rt s) (static cs rt t)
    | TForall _ -> typ
    | TTop -> 
        if RTSet.equal rt rtany then
          TTop
        else (* TODO: no arrow type that is the supertype of all arrows *)
          List.fold_left (simpl_static cs) TBot (RTSetExt.to_list rt)
    | TId _ -> typ
    | TRec (x, t) -> let t' = TRec (x, static cs rt t) in
                     (match t' with TRec (_, TBot) -> TBot | typ -> typ)
    | TLambda _ -> failwith "TLambda in static"
    | TFix _ -> failwith "TLambda in static"
    | TApp _ -> typ

  let rec set_global_object env cname =
    let ci = IdMap.find cname env.typ_ids in
    match ci with
      | TRef (TObject fs), KStar ->
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
    | EnvType (p, x, writ_typ) ->
      if IdMap.mem x env.Env.typ_ids then
	raise (Not_wf_typ (sprintf "the type %s is already defined" x))
      else
	let t = desugar_typ p writ_typ in
	let k = Env.kind_check env t in
	{ env with 
	  Env.typ_ids = IdMap.add x (t, k) env.Env.typ_ids }
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

    | TObject flds1, TObject flds2 ->
      List.fold_left assoc_merge
	IdMap.empty
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


let fields = Env.fields

let typid_env env = IdMap.map (fun (t, _) -> t) env.Env.typ_ids

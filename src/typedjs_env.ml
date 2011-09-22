open Prelude
open Typedjs_syntax

module List = ListExt
exception Not_wf_typ of string

let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)

let desugar_typ = Sb_desugar.desugar_typ

(* Necessary for equi-recursive subtyping. *)
module TypPair = struct
  type t = typ * typ
  let compare = Pervasives.compare
end

module TPSet = Set.Make (TypPair)
module TPSetExt = SetExt.Make (TPSet)


(* Pair that were mismatched *)
type subtype_exn =
  | ExtraFld of field
  | MismatchTyp of typ * typ
  | MismatchFld of field * field

exception Not_subtype of subtype_exn


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

  let kind_check env (typ : typ) : kind  =
    Sb_kinding.kind_check (IdMap.map (fun (_, k) -> k) env.typ_ids) typ

  let bind_id x t env  = { env with id_typs = IdMap.add x t env.id_typs }

  let bind_lbl x t env = { env with lbl_typs = IdMap.add x t env.lbl_typs }

  let bind_typ_id (x : id) (t : typ) (env : env) = 
    let k = kind_check env t in
    { env with 
      typ_ids = IdMap.add x (t, k) env.typ_ids }

  let lookup_id x env = IdMap.find x env.id_typs

  let lookup_lbl x env = IdMap.find x env.lbl_typs

  let lookup_typ_id x env = IdMap.find x env.typ_ids

  let id_env env = env.id_typs

  let clear_labels env = { env with lbl_typs = 
      try 
        IdMap.add "%return" (IdMap.find "%return" env.lbl_typs) IdMap.empty
      with Not_found -> IdMap.empty }

  let dom env = IdSetExt.from_list (IdMapExt.keys env.id_typs)

  let rec bind_typ env typ : env * typ = match typ with
    | TForall (x, s, t) -> bind_typ (bind_typ_id x s env) t
    | typ -> (env, typ)

  let simpl_typ env typ = TypImpl.simpl_typ env.typ_ids typ

  let expose env typ = TypImpl.expose env.typ_ids typ


  let inherits p env obj_typ idx_pat = 
    TypImpl.inherits p env.typ_ids obj_typ idx_pat

  let subtypes env ss ts = TypImpl.subtypes env.typ_ids ss ts
  
  let subtype env = TypImpl.subtype env.typ_ids

  let typ_union env = TypImpl.typ_union env.typ_ids

  let typ_intersect env = TypImpl.typ_intersect env.typ_ids

  let assert_subtyp env pos s t = TypImpl.assert_subtyp env.typ_ids pos s t

  let simpl_static env (typ : typ) (rt : RT.t) : typ = match rt with
    | RT.Num -> typ_union env (TPrim Num) typ
    | RT.Re _ -> typ_union env (TRegex any_fld) typ
    | RT.Bool -> typ_union env typ_bool typ
    | RT.Function ->
      typ_union env (TObject (mk_obj_typ
				[(proto_pat, PPresent (TId "Function"))])) typ 
    | RT.Object -> 
      typ_union env (TObject (mk_obj_typ
				[proto_pat, PPresent (TId "Object")])) typ
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
    let ci = 
      try IdMap.find cname env.typ_ids
      with Not_found -> 
	raise (Not_wf_typ ("global object, " ^ cname ^ ", not found")) in
    match ci with
      | TRef (TObject o), KStar ->
	let fs = fields o in
        let add_field env ((x : pat), (p : prop)) = 
	  begin match P.singleton_string x, p with
            | (Some s, PPresent t) -> bind_id s (TRef t) env
            | _, PAbsent -> env
            | _ -> raise (Not_wf_typ (cname ^ " field was a regex in global"))
        end in
        List.fold_left add_field env fs
      | _ -> 
        raise (Not_wf_typ (cname ^ " global must be an object"))

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
      if IdMap.mem x env.id_typs then
        raise (Not_wf_typ (x ^ " is already bound in the environment"))
      else
        bind_id x (desugar_typ p typ) env
    | EnvType (p, x, writ_typ) ->
      if IdMap.mem x env.typ_ids then
	raise (Not_wf_typ (sprintf "the type %s is already defined" x))
      else
	let t = desugar_typ p writ_typ in
	let k = kind_check env t in
	{ env with 
	  typ_ids = IdMap.add x (t, k) env.typ_ids }
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
  let fn x typ cf_env = L.bind x (L.runtime tc_env.typ_ids typ) cf_env in
    IdMap.fold fn (id_env tc_env) L.empty_env

let operator_env_of_tc_env tc_env =
  let fn x t env = IdMap.add x (df_func_of_typ (syns tc_env) t) env in
    IdMap.fold fn (id_env tc_env) IdMap.empty
*)




let apply_subst subst typ = IdMap.fold typ_subst subst typ


(* Quick hack to infer types; it often works. Sometimes it does not. *)
let assoc_merge = IdMap.merge (fun x opt_s opt_t -> match opt_s, opt_t with
  | Some (TId y), Some (TId z) -> 
    if x = y then opt_t else opt_s
  | Some (TId _), Some t 
  | Some t, Some (TId _) -> Some t
  | Some t, _
  | _, Some t ->
    Some t
  | None, None -> None)


let rec typ_assoc (env : env) (typ1 : typ) (typ2 : typ) = 
  match (typ1, typ2) with
    | TId x, _ -> IdMap.singleton x typ2
    | TApp (s1, [s2]), TApp (t1, [t2])
    | TIntersect (s1, s2), TIntersect (t1, t2)
    | TUnion (s1, s2), TUnion (t1, t2) -> 
      assoc_merge (typ_assoc env s1 t1) (typ_assoc env s2 t2)

    | TApp (s1, s2), t
    | t, TApp (s1, s2) ->
      typ_assoc env (simpl_typ env (TApp (s1, s2))) t

    | TObject o1, TObject o2 ->
      let flds1 = fields o1 in
      let flds2 = fields o2 in
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


let tid_env env = env.typ_ids

let typid_env env = IdMap.map (fun (t, _) -> t) env.typ_ids

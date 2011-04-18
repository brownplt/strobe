open Prelude
open Typedjs_syntax
open RegLang_syntax

exception Not_wf_typ of string

let string_of_re = RegLang_syntax.Pretty.string_of_re

(* Necessary for equi-recursive subtyping. *)
module TypPair = struct
  type t = typ * typ
  let compare = Pervasives.compare
end

module TPSet = Set.Make (TypPair)
module TPSetExt = SetExt.Make (TPSet)

module ObjLang = struct
  type t = typ * RegLang.fsm
  let compare = Pervasives.compare
end

module OLSet = Set.Make (ObjLang)
module OLSetExt = SetExt.Make (OLSet)

let rec typ_subst x s typ = 
(* printf "Substing: %s %s %s\n\n" x (string_of_typ s) (string_of_typ typ);*)
match typ with
  | TPrim _ -> typ
  | TRegex _ -> typ
  | TId y -> if x = y then s else typ
  | TSyn x -> typ
  | TUnion (t1, t2) -> TUnion (typ_subst x s t1, typ_subst x s t2)
  | TIntersect (t1, t2) ->
      TIntersect (typ_subst x s t1, typ_subst x s t2)
  | TArrow (t2s, t3)  ->
      TArrow (map (typ_subst x s) t2s, typ_subst x s t3)
  | TObject (fs, proto) -> 
      let prop_subst p = match p with
        | PPresent typ -> PPresent (typ_subst x s typ)
        | PMaybe typ -> PMaybe (typ_subst x s typ)
        | PAbsent -> PAbsent 
        | PErr -> PErr in
      TObject (map (second2 prop_subst) fs, typ_subst x s proto)
  | TRef t -> TRef (typ_subst x s t)
  | TSource t -> TSource (typ_subst x s t)
  | TSink t -> TSink (typ_subst x s t)
  | TTop -> TTop
  | TBot -> TBot
  | TField -> TField
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


module Env = struct

  type class_info = {
    fields : typ IdMap.t;
    sup : constr option
  }

  type env = {
    id_typs : typ IdMap.t; 
    lbl_typs : typ IdMap.t;
    typ_ids: typ IdMap.t; (* bounded type variables *)
    typ_syns : typ IdMap.t; (* type synonyms *)
  }


  let empty_env = { 
    id_typs = IdMap.empty;
    lbl_typs = IdMap.empty;
    typ_ids = IdMap.empty;
    typ_syns = IdMap.empty;
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

  let rec app_typ env (t : typ) = match t with
    | TApp (t1, t2) ->
        begin match t1 with
          | TSyn x -> 
              let typ = IdMap.find x env.typ_syns in
                app_typ env (TApp (typ, t2))
          | TForall (x, s, t) -> typ_subst x t2 t
          | _ -> raise (Not_wf_typ (sprintf "Expected a quantified type in TApp"))
        end
    | _ -> raise (Not_wf_typ ("Can only apply TApp"))

  let rec subt env (cache : TPSet.t) s t : TPSet.t= 
    if TPSet.mem (s, t) cache then
      cache
    else if s = t then
      cache
    else
      let subtype = subt env in
      let cache = TPSet.add (s, t) cache in
      match s, t with
        | TApp _, _ ->
            subtype cache (app_typ env s) t
        | _, TApp _ ->
            subtype cache s (app_typ env t)
        | TSyn x, _ -> subtype cache (IdMap.find x env.typ_syns) t
        | _, TSyn y -> subtype cache s (IdMap.find y env.typ_syns)
        | TPrim Int, TPrim Num -> cache
        | TRegex (_, fsm1), TRegex (_, fsm2) ->
            if RegLang.contains fsm1 fsm2 then cache 
            else raise Not_subtype
        | TRegex _, TPrim Str -> cache
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
        | TObject (fs1, p1), TObject (fs2, p2) -> 
            subtype (subtype_object' env cache fs1 fs2) p1 p2
        | TRef s', TRef t' -> subtype (subtype cache s' t') t' s'
        | TSource s, TSource t -> subtype cache s t
        | TSink s, TSink t -> subtype cache t s
        | TRef s, TSource t -> subtype cache s t
        | TRef s, TSink t -> subtype cache t s
        | TForall (x1, s1, t1), TForall (x2, s2, t2) -> 
            let (env', typ') = bind_typ env s in
            let (env'', typ'') = bind_typ env t in
              subt env cache typ' typ''
        | _, TTop -> cache
        | TBot, _ -> cache
        | _ -> raise Not_subtype

  and subtype_object' env cache fs1 fs2 =
    let rec check_prop (((re2, m_j), g_j) as p2) (p2s, fs, cache) =
      match fs with
        | [] -> (p2::p2s, fs, cache)
        | (((re1, l_i), f_i) as p1)::rest -> 
          match RegLang.overlap l_i m_j with 
            | true -> 
              let cache' = subtype_prop env cache f_i g_j in
              let p2' = ((re1, RegLang.subtract m_j l_i), g_j) in
              let p2'', fs1', cache'' = check_prop p2' (p2s, rest, cache') in
              p2'', ((re1, RegLang.subtract l_i m_j), f_i)::fs1', cache''
            | false -> 
              let p2', fs1', cache' = check_prop p2 (p2s, rest, cache) in
              p2', p1::fs1', cache' in
    let (fs2', fs1', cache') = 
      List.fold_right check_prop fs2 ([], fs1, cache) in
    if List.for_all (fun ((_, fsm), _) -> RegLang.is_empty fsm) fs1' then
      if List.for_all (fun ((_, fsm), g_j) -> 
        (match g_j with 
          | PPresent _ -> false
          | _ -> true) || RegLang.is_empty fsm) fs2' then
        cache
      else raise Not_subtype
    else raise Not_subtype

  (* S-Object, "Algorithmic Subtyping of Objects" from the paper *)
  and subtype_object env cache fs1 fs2 = 
    printf "Subtyping objects\n%!";
    let all_fields fs = List.fold_right 
      (fun ((rei, fsmi), fi) l' -> match fi with
        | PAbsent -> l'
        | _ -> printf "Unioning...\n%!"; RegLang.union l' fsmi)
      fs (RegLang.fsm_of_regex RegLang_syntax.Empty) in
    let l = all_fields fs1 in
    let m = all_fields fs2 in
    (* Check for containment of union_i(L_i) < union_j(M_j) *)
    printf "Counterexampling...\n%!";
    match RegLang.counterexample l m with
      | Some str -> raise Not_subtype
      | None ->
        (* forall i,j, if L_i overlaps M_j, F_i <: G_j *)
        (List.fold_right
           (fun ((_, l_i), f_i) cache ->
             List.fold_right
               (fun ((_, m_j), g_j) cache ->
                 printf "Overlapping...\n";
                match RegLang.overlap l_i m_j with
                  | true -> subtype_prop env cache f_i g_j
                  | false -> cache)
               fs2 cache)
           fs1
          (* forall j, if M_j - union_i(L_i) isn't empty, it is maybe or err *)
           (List.fold_right
              (fun ((_, m_j), g_j) cache ->
                printf "Internal counterexampling...\n%!";
                match RegLang.counterexample m_j l with
                  | None -> cache
                  | Some str -> match g_j with
                      | PPresent _ -> raise Not_subtype
                      | PMaybe _ 
                      | PAbsent
                      | PErr _ -> cache)
              fs2 cache))

  (* subtype_prop encodes this lattice:
          PErr
           |
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
    | _, PErr -> cache
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

  and normalize_typ env typ = match typ with
    | TPrim _ -> typ
    | TApp (t1, t2) -> TApp (normalize_typ env t1, normalize_typ env t2)
    | TUnion (s, t) -> 
        typ_union env (normalize_typ env s) (normalize_typ env t)
    | TIntersect (s, t) -> 
        typ_intersect env (normalize_typ env s) (normalize_typ env t)
    | TRegex _ -> typ
    | TObject (fs, proto) ->
        let the_fsms = map snd2 (map fst2 fs) in
        let rec f fsms = match fsms with
          | [] -> None
          | fsm1::rest1 ->
              let rec g fsms2 = match fsms2 with
                | [] -> None
                | fsm2::rest2 when fsm1 != fsm2 ->
                    begin match RegLang.overlap_example fsm1 fsm2 with
                      | Some chs -> Some chs
                      | None -> g rest2
                    end 
                | _ -> None in
                match g the_fsms with
                  | Some str -> Some str
                  | None -> f rest1 in
          begin match f the_fsms with
            | Some s -> 
                raise (Not_wf_typ 
                         (sprintf "The string %s may inhabit multiple fields" s))
            | None ->
                TObject (map (second2 (normalize_prop env)) fs, proto)
          end
    | TArrow (args, result) ->
        TArrow (map (normalize_typ env) args,
                normalize_typ env result)
    | TRef t -> TRef (normalize_typ env t)
    | TSource t -> TSource (normalize_typ env t)
    | TSink t -> TSink (normalize_typ env t)
    | TTop -> TTop
    | TBot -> TBot
    | TField -> TField
    | TId x ->
        if IdMap.mem x env.typ_ids then typ
        else raise (Not_wf_typ ("the type variable " ^ x ^ " is unbound"))
    | TForall (x, s, t) -> 
        let s = normalize_typ env s in
          TForall (x, s, normalize_typ (bind_typ_id x s env) t)
    | TRec (x, t) ->
      TRec (x, normalize_typ (bind_typ_id x typ env) t)
    | TSyn x ->
      if IdMap.mem x env.typ_syns then typ
      else raise (Not_wf_typ (x ^ " is not a type"))
  and normalize_prop env prop = match prop with
    | PPresent typ -> PPresent (normalize_typ env typ)
    | PMaybe typ -> PMaybe (normalize_typ env typ)
    | PAbsent -> PAbsent
    | PErr -> PErr

  let check_typ p env t = 
    try normalize_typ env t
    with Not_wf_typ s -> raise (Typ_error (p, s))
      | Not_found -> failwith "Not found in check_typ!!!"

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
    | RT.Function -> typ_union env (mk_object_typ [] None (TSyn "Function")) typ
    | RT.Object -> typ_union env (mk_object_typ [] None (TSyn "Object")) typ
    | RT.Undefined -> typ_union env (TPrim Undef) typ

  let rtany = 
    RTSetExt.from_list
    [ RT.Num; RT.Re any_str; RT.Bool; RT.Function; RT.Object; RT.Undefined ]

  let is_re rt = match rt with RT.Re _ -> true | _ -> false

  let match_re rts = if RTSet.cardinal rts = 1 then
      match RTSet.choose rts with
        | RT.Re re -> Some re
        | _ -> None
    else None

  let rec static cs (rt : RTSet.t) (typ : typ) : typ = match typ with
    | TBot -> TBot (* might change if we allow arbitrary casts *)
    | TArrow _ -> if RTSet.mem RT.Function rt then typ else TBot
    | TPrim Str
    | TRegex _ -> if RTSet.exists is_re rt then 
        match match_re rt with
          | Some re -> TRegex (re, RegLang.fsm_of_regex re)
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
    | TField -> List.fold_left (basic_static cs) TBot (RTSetExt.to_list rt)
    | TTop -> 
        if RTSet.equal rt rtany then
          TTop
        else (* TODO: no arrow type that is the supertype of all arrows *)
          List.fold_left (basic_static2 cs) TBot (RTSetExt.to_list rt)
    | TId _ -> typ
    | TRec (x, t) -> let t' = TRec (x, static cs rt t) in
                     (match t' with TRec (_, TBot) -> TBot | typ -> typ)
    | TSyn _ -> typ
    | TApp _ -> typ

  let rec set_global_object env cname =
    let ci = IdMap.find cname env.typ_syns in
    match ci with
      | TRef (TObject (fs, _)) ->
        let add_field env ((x : field), (p : prop)) = begin match x, p with
          | ((RegLang_syntax.String s, _), PPresent t) -> bind_id s (TRef t) env
          | _, PAbsent -> env
          | _ -> raise (Not_wf_typ (cname ^ " field was a regex in global"))
        end in
        List.fold_left add_field env fs
      | _ -> 
        raise (Not_wf_typ (cname ^ " global must be an object"))

  let syns env = env.typ_syns

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
    | EnvBind (x, typ) ->
      if IdMap.mem x env.Env.id_typs then
        raise (Not_wf_typ (x ^ " is already bound in the environment"))
      else
        Env.bind_id x typ env
    | EnvClass (x, _, t) ->
      if IdMap.mem x env.Env.typ_syns then
        raise (Not_wf_typ ("the type " ^ x ^ " is already defined"))
      else
        { env with Env.typ_syns = IdMap.add x t env.Env.typ_syns }
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

let typ_unfold typ = match typ with
    | TRec (x, t) -> typ_subst x typ t
    | _ -> typ

let rec simpl_typ env typ = 
  let typ = match typ with
    | TSyn x -> Env.normalize_typ env (IdMap.find x env.Env.typ_syns)
        (* normalization => success *)
    | TApp _ -> Env.app_typ env typ
    | _ -> typ
  in typ_unfold typ


let apply_subst subst typ = IdMap.fold typ_subst subst typ

 (* TODO: occurs check *)
let rec unify env (subst : typ IdMap.t option) (s : typ) (t : typ)  = 
  match s, t with
  | TPrim s, TPrim t -> 
      if s = t then subst 
      else None 
  | TId x, TId y -> 
      if x = y then subst 
      else None
  | TId x, t -> 
      begin match subst with
        | None -> None
        | Some subst' -> 
            if IdMap.mem x subst' then
              begin
                let s = IdMap.find x subst' in
                  Some (IdMap.add x (apply_subst subst' 
                                       (Env.typ_union env s t)) subst')
              end
            else
              Some (IdMap.add x (apply_subst subst' t) subst')
            end
  | s, TId y -> unify env subst (TId y) s
  | TUnion (s1, s2), TUnion (t1, t2) -> unify env (unify env subst s1 t1) s2 t2
  | TIntersect (s1, s2), TIntersect (t1, t2) -> 
      unify env (unify env subst s1 t1) s2 t2
  | TArrow (s2s, s3), TArrow (t2s, t3) ->
      if List.length s2s != List.length t2s then None 
      else List.fold_left2 (unify env) subst (s3 :: s2s) (t3 :: t2s)
  | TApp (s1, s2), TApp (t1, t2) -> 
      unify env (unify env subst s1 t1) s2 t2
  | TSyn x1, TSyn x2 -> if x1 = x2 then subst else None
  | TObject (fs1, p1), TObject (fs2, p2) ->
      if List.length fs1 != List.length fs2 then None
      else
        let f subst (x, p1) (y, p2) = 
          if x = y then
            match p1, p2 with
              | PPresent s, PPresent t -> unify env subst s t
              | PMaybe s, PMaybe t -> unify env subst s t
              | PAbsent, PAbsent -> subst
              | _ -> None
          else None in
        let subst' = List.fold_left2 f subst fs1 fs2 in
          unify env subst' p1 p2
  | TRef s, TRef t -> unify env subst s t
  | TSource s, TSource t -> unify env subst s t
  | TSink s, TSink t -> unify env subst s t
  | TTop, TTop -> subst
  | TBot, TBot -> subst
  | TForall _, TForall _ -> None
  | _ -> None


let unify_typ env (s : typ) (t : typ) : typ IdMap.t option = 
  unify env (Some IdMap.empty) s t

let rec fields p env obj fsm = 
  let rec fields p env cache obj fsm =
    if OLSet.mem (obj, fsm) cache then TBot
    else 
      let cache = OLSet.add (obj, fsm) cache in
      match obj, fsm with
        | _, fsm when RegLang.is_empty fsm -> TBot
        | TRec (x, t), _ -> 
          fields p env cache (typ_subst x obj t) fsm
        | TObject (fs, proto), _ ->
             (** Since the fsms in fs are disjoint, we can safely subtract
                 them from the fsm in the regex without worrying about
                 missing overlaps in the cases where we know the field must
                 be present.  merge_prop iterates over fields and builds a
                 type and a more restricted fsm to use on prototype lookup *)
          let merge_prop ((re, fsm), prop) (fsm', typ) =
            match RegLang.overlap_example fsm fsm' with
              | None -> (fsm', typ) 
              | Some str -> match prop with
                     (** If the property is present, we subtract the fsm *)
                  | PPresent s -> 
                    let fsm'' = RegLang.subtract fsm' fsm in
                    let typ' = Env.typ_union env s typ in
                    (fsm'', typ')
                     (** If the property is possibly present, we can't 
                         subtract the fsm, but we must include the type *)
                  | PMaybe s ->
                    (fsm', Env.typ_union env s typ)
                  | PAbsent -> (fsm', typ) 
                  | PErr -> 
                    raise 
                      (Typ_error 
                         (p, sprintf "Looked up bad property %s, the overlap was %s"
                           (RegLang_syntax.Pretty.string_of_re re)
                           str))
          in
          let (proto_fsm, top_typ) = List.fold_right merge_prop fs (fsm, TBot) in
          begin match simpl_typ env proto with
            | TRef (TObject _ as tproto) -> 
              Env.typ_union env top_typ (fields p env cache tproto proto_fsm)
            | TPrim Null -> if RegLang.is_empty proto_fsm then
                top_typ else Env.typ_union env top_typ (TPrim Undef)
            | _ -> raise (Typ_error (p, sprintf "Bad type for proto: %s" 
              (string_of_typ proto)))
          end
        | _, _ -> failwith ("Bad fields invocation" ^ (string_of_typ obj))
  in fields p env OLSet.empty obj fsm

open Prelude
open Typedjs_syntax
open FormatExt
open TypImpl

module List = ListExt
exception Not_wf_typ of string

let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)

let desugar_typ = Sb_desugar.desugar_typ

type env = {
  id_typs : typ IdMap.t; (* type of term identifiers *)
  lbl_typs : typ IdMap.t; (* types of labels *)
  typ_ids: (typ * kind) IdMap.t; (* bounded type variables *)
}

let print_env env fmt : unit =
  let unname t = if (!TypImpl.Pretty.useNames) then replace_name None t else t in
  vert [text "Types of term identifiers:";
        vert (List.map (fun (id, t) -> 
          horz [text id; text "="; (TypImpl.Pretty.typ (unname t))]) (IdMapExt.to_list env.id_typs));
        empty; 
        text "Primitive types:";
        vert (List.map text (Sb_kinding.list_prims ()));
        empty; 
        text "Types of labels:";
        vert (List.map (fun (id, t) -> horz[text id; text "="; (TypImpl.Pretty.typ (unname t))]) 
                (IdMapExt.to_list env.lbl_typs));
        empty; 
        text "Bounded type variables:";
        vert (List.map (fun (id, (t, k)) -> 
          horz [text id; 
                vert [horz [text "="; (TypImpl.Pretty.typ (unname t))];
                      horz [text "::"; TypImpl.Pretty.kind k]]]) (IdMapExt.to_list env.typ_ids));
        empty
       ] 
    fmt;
  Format.pp_print_flush fmt ()


let empty_env = { 
  id_typs = IdMap.empty;
  lbl_typs = IdMap.empty;
  typ_ids = IdMap.empty;
}

let kind_check env recIds (typ : typ) : kind  =
  Sb_kinding.kind_check (IdMap.map (fun (_, k) -> k) env.typ_ids) recIds typ

let bind_id x t env  = { env with id_typs = IdMap.add x t env.id_typs }

let bind_lbl x t env = { env with lbl_typs = IdMap.add x t env.lbl_typs }

let bind_rec_typ_id (x : id) recIds (t : typ) (env : env) = 
  let k = kind_check env recIds t in
  { env with 
    typ_ids = IdMap.add x (t, k) env.typ_ids }

let bind_typ_id x t env = bind_rec_typ_id x [] t env

let bind_recursive_types (xts : (id * typ) list) (env : env) =
  let typ_ids' = List.fold_left (fun ids (x, t) -> IdMap.add x (t, KStar) ids) env.typ_ids xts in
  let env' = {env with typ_ids = typ_ids'} in
  timefn "Bindrec/Kind checking" (List.fold_left (fun env (x, t) -> bind_typ_id x t env) env') xts

let unchecked_bind_typ_ids (xts : (id * typ) list) (env : env) =
  let typ_ids' = List.fold_left (fun ids (x, t) -> IdMap.add x (t, KStar) ids) env.typ_ids xts in
  {env with typ_ids = typ_ids'}
  

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
  | TForall (n, x, s, t) -> bind_typ (bind_typ_id x s env) (apply_name n t)
  | typ -> (env, typ)

let simpl_typ env typ = TypImpl.simpl_typ env.typ_ids typ

let expose env typ = TypImpl.expose env.typ_ids typ


let inherits p env obj_typ idx_pat = 
  TypImpl.inherits p env.typ_ids obj_typ idx_pat

let subtypes env ss ts = TypImpl.subtypes env.typ_ids ss ts

let subtype env = TypImpl.subtype env.typ_ids

let typ_union env = TypImpl.typ_union env.typ_ids

let typ_intersect env = TypImpl.typ_intersect env.typ_ids

let simpl_static env (typ : typ) (rt : RT.t) : typ = match rt with
  | RT.Num -> typ_union env (TPrim "Num") typ
  | RT.Re _ -> typ_union env (TRegex any_fld) typ
  | RT.Bool -> typ_union env typ_bool typ
  | RT.Function ->
    (* TODO(arjun): This should be a type operator invocation *)
    typ_union env (TObject (mk_obj_typ
                              [(proto_pat, Present, TId "Function")] P.empty)) typ 
  | RT.Object -> (* TODO(arjun): This should be a type operator invocation *)
    typ_union env (TObject (mk_obj_typ
                              [(proto_pat, Present, TId "Object")] P.empty)) typ
  | RT.Undefined -> typ_union env (TPrim "Undef") typ

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
  | TPrim "Num"
  | TPrim "True"
  | TPrim "False" -> if RTSet.mem RT.Bool rt then typ else TBot
  | TPrim "Null" -> if RTSet.mem RT.Object rt then typ else TBot
  | TPrim "Undef" -> 
    if RTSet.mem RT.Undefined rt then typ else TBot
  | TPrim "Unsafe" -> TPrim "Unsafe"
  | TPrim s -> failwith ("**" ^ s ^ "**")
  (* any other app will be an object from a constructor *)
  | TRef (_, TObject _) -> if RTSet.mem RT.Object rt then typ else TBot
  | TObject _ -> failwith "Static got a functional object"
  | TWith _ -> failwith "Static got a TWith"
  | TRef (n, t) -> TRef (n, t)
  | TSource (n, t) -> TSource (n, t)
  | TSink (n, t) -> TSink (n, t)
  | TThis t -> TThis t
  | TUnion (n, s, t) -> apply_name n (typ_union cs (static cs rt s) (static cs rt t))
  | TIntersect (n, s, t) -> apply_name n (typ_intersect cs (static cs rt s) (static cs rt t))
  | TForall _ -> typ
  | TTop -> 
    if RTSet.equal rt rtany then
      TTop
    else (* TODO: no arrow type that is the supertype of all arrows *)
      List.fold_left (simpl_static cs) TBot (RTSetExt.to_list rt)
  | TId _ -> typ
  | TRec (n, x, t) -> let t' = TRec (n, x, static cs rt t) in
                   (match t' with TRec (_, _, TBot) -> TBot | typ -> typ)
  | TLambda _ -> failwith "TLambda in static"
  | TFix _ -> failwith "TLambda in static"
  | TApp _ -> typ
  | TUninit t -> match !t with
    | None -> static cs rt (TPrim "Undef")
    | Some t -> static cs rt t

let rec set_global_object env cname =
  let (ci_typ, ci_kind) = 
    try IdMap.find cname env.typ_ids
    with Not_found -> 
      raise (Not_wf_typ ("global object, " ^ cname ^ ", not found")) in
  match expose env (simpl_typ env ci_typ), ci_kind with
    | TRef (n, TObject o), KStar ->
      let fs = fields o in
      let add_field env (x, pres, t) =
        if pres = Present then
          match P.singleton_string x with
            | Some s -> bind_id s (TRef (n, t)) env
            | None -> 
              raise (Not_wf_typ (cname ^ " field was a regex in global"))
        else
          raise (Not_wf_typ "all fields on global must be present") in
      List.fold_left add_field env fs
    | t, _ -> raise (Not_wf_typ (cname ^ " global must be an object, got\n" ^
                                   string_of_typ t))

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
  let rec add recIds env decl = match decl with
    | EnvBind (p, x, typ) ->
      if IdMap.mem x env.id_typs then
        raise (Not_wf_typ (x ^ " is already bound in the environment"))
      else
        let t = expose_twith env.typ_ids (desugar_typ p typ) in
        (* Printf.eprintf "Binding type for %s to %s\n" x (string_of_typ t); *)
        bind_id x t env
    | EnvType (p, x, writ_typ) ->
      if IdMap.mem x env.typ_ids then
        raise (Not_wf_typ (sprintf "the type %s is already defined" x))
      else
        let t = expose_twith env.typ_ids (desugar_typ p writ_typ) in
        (* Printf.eprintf "Binding %s to %s\n" x (string_of_typ (apply_name (Some x) t)); *)
        let k = kind_check env recIds t in
        { env with 
          typ_ids = IdMap.add x (apply_name (Some x) t, k) env.typ_ids }
    | EnvPrim (p, s) ->
      Sb_kinding.new_prim_typ  s;
      env
    | ObjectTrio(pos, (c_id, c_typ), (p_id, p_typ), (i_id, i_typ)) ->
      (* add prototype field to constructor *)
      let c_typ = expose_twith env.typ_ids (desugar_typ pos c_typ) in
      let c_absent_pat = match c_typ with TRef(_, TObject(f)) -> absent_pat f | _ -> P.all in
      let constructor_with = TWith(c_typ, (mk_obj_typ 
                                             [P.singleton "prototype", Present, 
                                              TApp(TPrim("Mutable"), [TId(p_id)])]
                                             (P.subtract c_absent_pat (P.singleton "prototype")))) in
      let constructor = replace_name (Some c_id) (expose_twith env.typ_ids constructor_with) in
      (* add constructor field to prototype *)
      let p_typ = (desugar_typ pos p_typ) in
      let p_typ = match p_typ with TId _ -> simpl_typ env p_typ | _ -> p_typ in
      let (prototype_added_fields, prototype_with) = match p_typ with 
        | TWith(base, f) ->
          (fields f), TWith(base, (mk_obj_typ
                                    ((P.singleton "constructor", Present, TId(c_id))::(fields f))
                                    (P.subtract (absent_pat f) (P.singleton "constructor"))))
        | TRef(_, TObject(f))
        | TSource(_, TObject(f)) ->
          let temp = 
            (expose_twith env.typ_ids 
               (TWith(TId("AnObject"),
                      (mk_obj_typ
                         [P.singleton "constructor", Present, TId(c_id)]
                         (P.subtract (absent_pat f) (P.singleton "constructor")))))) in
          (fields f), TWith(temp, (mk_obj_typ (fields f) (P.subtract (absent_pat f) (P.singleton "constructor"))))
        | _ -> failwith "impossible" in
      let prototype = match expose_twith env.typ_ids prototype_with with TRef (n, t) -> TSource(n, t) | t -> t in
      (* add __proto__ field to instance *)
      let i_typ = (desugar_typ pos i_typ) in
      let i_typ = match i_typ with TId _ -> simpl_typ env i_typ | _ -> i_typ in
      let instance_with = 
        let proto_fields = List.map (fun (n, _, t) -> (n, Inherited, t)) prototype_added_fields in
        let proto_field_pat = P.unions (proto_pat::(List.map fst3 prototype_added_fields)) in
        match i_typ with 
        | TWith(base, f) ->
          let absent_pat = absent_pat f in
          let new_fields = List.map (fun (pat, p, t) ->
            (P.subtract (P.subtract pat proto_field_pat) absent_pat, p, t))
            (fields f) in
          TWith(base, mk_obj_typ ((proto_pat, Present, TId(p_id))::proto_fields@new_fields) absent_pat)
        | TRef(_, TObject(f))
        | TSource(_, TObject(f)) ->
          let absent_pat = P.subtract (absent_pat f) proto_field_pat in
          let base_fields = List.map (fun (pat, p, t) ->
            (P.subtract (P.subtract pat proto_field_pat) absent_pat, p, t))
            (fields f) in
          TWith(TId "AnObject",
                (mk_obj_typ ((proto_pat, Present, TId(p_id))::proto_fields@base_fields) absent_pat))
        | _ -> failwith "impossible" in
      let instance = replace_name (Some i_id) (expose_twith env.typ_ids instance_with) in
      let (k_c, k_p, k_i) = (kind_check env [c_id; p_id; i_id] constructor,
                             kind_check env [c_id; p_id; i_id] prototype,
                             kind_check env [c_id; p_id; i_id] instance) in
      { env with 
        typ_ids = 
          (IdMap.add c_id (constructor, k_c)
             (IdMap.add p_id (prototype, k_p)
                (IdMap.add i_id (instance, k_i) env.typ_ids))) }
    | RecBind (binds) ->
      let ids = List.concat (List.map (fun b -> match b with
        | EnvBind (_, x, _) -> [x]
        | EnvType (_, x, _) -> [x]
        | ObjectTrio(_, (c, _), (p, _), (i, _)) -> [c;p;i]
        | EnvPrim _
        | RecBind _ -> []) binds) in
      Printf.eprintf "Recursively including ids: ";
      List.iter (fun x -> Printf.eprintf "%s " x) ids;
      List.fold_left (add ids) env binds
  in List.fold_left (add []) env lst



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
    | TIntersect (_, s1, s2), TIntersect (_, t1, t2)
    | TUnion (_, s1, s2), TUnion (_, t1, t2) -> 
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
    | TSource (_, s), TSource (_, t)
    | TSink (_, s), TSink (_, t)
    | TRef (_, s), TRef (_, t) ->
      typ_assoc env s t
    | TArrow (args1, v1, r1), TArrow (args2, v2, r2) ->
      List.fold_left assoc_merge
        ((fun base -> match v1, v2 with
        | Some v1, Some v2 -> assoc_merge (typ_assoc env v1 v2) base
        | _ -> base)
            (typ_assoc env r1 r2))
        (List.map2_noerr (typ_assoc env) args1 args2)
    | TRec (_, x, s), TRec (_, y, t) ->
      (* could do better here, renaming*)
      typ_assoc env s t
    | TForall (_, x, s1, s2), TForall (_, y, t1, t2) ->
      (* also here *)
      assoc_merge (typ_assoc env s1 t1) (typ_assoc env s2 t2)
    | _ -> IdMap.empty

and fld_assoc env (_, _, s) (_, _, t) = typ_assoc env s t

let tid_env env = env.typ_ids

let typid_env env = IdMap.map (fun (t, _) -> t) env.typ_ids


let extend_env (trm_vars : typ IdMap.t) (typ_vars : (typ * kind) IdMap.t) env =
  let merge_fn toStr x left right = match (left, right) with
    | Some t1, Some t2 -> failwith (sprintf "rebinding %s in the environment: currently has type %s and trying to add type %s" x (toStr t1) (toStr t2))
    | None, Some t
    | Some t, None -> Some t
    | None, None -> failwith "impossible case in extend_env" in
  { env with id_typs = IdMap.merge (merge_fn string_of_typ) env.id_typs trm_vars;
    typ_ids = IdMap.merge (merge_fn (fun (t, _) -> string_of_typ t)) env.typ_ids typ_vars }

let verify_env env : unit =
  let errors = ref false in
  let kinding_env = IdMap.map (fun (_, k) -> k) env.typ_ids in
  let f x (t, k) =
    let k' = Sb_kinding.kind_check kinding_env [] t in
    if k = k' then
      ()
    else
      begin
        printf "%s declared kind is %s, but calculated kind is %s.\n\
                Type of %s is:\n%s\n"
          x (string_of_kind k) (string_of_kind k') x (string_of_typ t);
        errors := true
      end in
  IdMap.iter f env.typ_ids
(* 
     if !errors then
     raise (Invalid_argument "ill-formed environment")
  *)

let kind_check env (typ : typ) : kind  =  kind_check env [] typ

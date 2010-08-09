open Prelude
open Typedjs_syntax
open Typedjs_types

exception Not_wf_typ of string

let string_of_typ = FormatExt.to_string Typedjs_syntax.Pretty.p_typ


let rec typ_subst x s typ = match typ with
  | TId y -> if x = y then s else typ
  | TConstr (c, ts) -> TConstr (c, map (typ_subst x s) ts)
  | TUnion (t1, t2) -> TUnion (typ_subst x s t1, typ_subst x s t2)
  | TArrow (t1, t2s, t3)  ->
      TArrow (typ_subst x s t1, map (typ_subst x s) t2s, typ_subst x s t3)
  | TObject fs -> TObject (map (second2 (typ_subst x s)) fs)
  | TObjStar (fs, cname, other_typ) ->
      TObjStar ((map (second2 (typ_subst x s)) fs), cname, typ_subst x s other_typ)
  | TRef t -> TRef (typ_subst x s t)
  | TSource t -> TSource (typ_subst x s t)
  | TSink t -> TSink (typ_subst x s t)
  | TTop -> TTop
  | TBot -> TBot
  | TField -> TField
  | TForall (y, t1, t2) -> 
      if x = y then 
        TForall (y, typ_subst x s t1, t2)
      else 
        failwith "TODO: capture-free substitution"
  | TRec (y, t) ->
      if x = y then
	failwith "TODO: capture-free (TRec)"
      else
	TRec (y, typ_subst x s t)

module Env = struct

  module TypPairOrderedType = struct
    type t = typ * typ
    let compare = Pervasives.compare
  end
    
  module TypSet = Set.Make (TypPairOrderedType)

  type class_info = {
    fields : typ IdMap.t;
    sup : constr option
  }

  type env = {
    id_typs : typ IdMap.t; 
    lbl_typs : typ IdMap.t;
    classes : class_info IdMap.t;
    subclasses : id IdMap.t; (* direct subclasses *)
    typ_ids: typ IdMap.t; (* bounded type variables *)
  }


  let empty_env = { 
    id_typs = IdMap.empty;
    lbl_typs = IdMap.empty;
    classes = IdMap.empty;
    subclasses = IdMap.empty;
    typ_ids = IdMap.empty;
  }

  let bind_id x t env  = { env with id_typs = IdMap.add x t env.id_typs }

  let bind_lbl x t env = { env with lbl_typs = IdMap.add x t env.lbl_typs }

  let bind_typ_id x t env = { env with typ_ids = IdMap.add x t env.typ_ids }

  let lookup_id x env = IdMap.find x env.id_typs

  let lookup_lbl x env = IdMap.find x env.lbl_typs

  let rec field_typ env cname fname = try
    let ci = IdMap.find cname env.classes in
      if IdMap.mem fname ci.fields then
        Some (IdMap.find fname ci.fields)
      else begin match ci.sup with
        | None -> None
        | Some cname' -> field_typ env cname' fname
      end
  with Not_found -> raise (Not_wf_typ ("undefined class: " ^ cname))

  let is_class env cname = IdMap.mem cname env.classes

  let id_env env = env.id_typs

  let clear_labels env = { env with lbl_typs = 
      try 
        IdMap.add "%return" (IdMap.find "%return" env.lbl_typs) IdMap.empty
      with Not_found -> IdMap.empty }

  let dom env = IdSetExt.from_list (IdMapExt.keys env.id_typs)

  let class_fields env constr = (IdMap.find constr env.classes).fields

  let cmp_props (k1, _) (k2, _) = match String.compare k1 k2 with
    | 0 -> raise (Not_wf_typ ("the field " ^ k1 ^ " is repeated"))
    | n -> n

  let rec r_subtype rel env s t =
    let st = r_subtype rel env in
      if (TypSet.mem (s,t) rel) then (printf "matching from set: (%s, %s)\n\n" (string_of_typ s) (string_of_typ t); true)
      else match s, t with
	| TId x, TId y -> x = y
	| TId x, t ->
	    let s = IdMap.find x env.typ_ids in
	      r_subtype rel env s t
	| TConstr (c1, []), TConstr (c2, []) ->
	    let rec is_subclass sub sup =
              if sub = sup then
		true
              else if not (IdMap.mem sub env.subclasses) then
		false (* sub is a root class *)
              else 
		is_subclass (IdMap.find sub env.subclasses) sup in
              is_subclass c1 c2
	| TObject fs1, TObject fs2 -> r_subtype_fields rel env fs1 fs2
	| TUnion (s1, s2), t -> st s1 t && st s2 t
	| s, TUnion (t1, t2) -> st s t1 || st s t2
	| TRef s', TRef t' -> st s' t' && st t' s'
        | TArrow (_, args1, r1), TArrow (_, args2, r2) ->
            r_subtypes rel env args2 args1 && r_subtype rel env r1 r2
        | TConstr (c_name, []), TObject fs2 ->
            (* Classes can be turned into objects. However, this drops
               all fields in the prototype. *)
            let fs1 = IdMapExt.to_list (IdMap.find c_name env.classes).fields in
            let fs1 = List.rev fs1 in
              r_subtype_fields rel env fs1 fs2
        | TObject fs1, TConstr (c_name, []) ->
            (* Same for the other direction. This must be double-checked. *)
            let fs2 = IdMapExt.to_list (IdMap.find c_name env.classes).fields in
            let fs2 = List.rev fs2 in
              r_subtype_fields rel env fs1 fs2
	| TObjStar (fs1, cname1, other_typ1), 
	    TObjStar (fs2, cname2, other_typ2) ->
	    r_subtype_fields rel env fs1 fs2 &&
	      cname1 = cname2 &&
	      st other_typ1 other_typ2
	| TObject fso, TObjStar (fs, cname, other_typ) ->
	    let all_fields = List.fast_sort cmp_props 
	      (List.rev (fs@(IdMapExt.to_list (class_fields env cname)))) in
	      r_subtype_star rel env fso all_fields other_typ
        | TSource s, TSource t -> st s t
        | TSink s, TSink t -> st t s
        | TRef s, TSource t -> st s t
        | TRef s, TSink t -> st t s
        | TConstr (constr, []), TField ->
            List.mem constr [ "Num"; "Int"; "Str"; "Undef"; "Bool" ]
	| _, TTop -> true
	| TBot, _ -> true
	| s, TRec (x, t') -> 
	    r_subtype (TypSet.add (s,t) rel) env s (typ_subst x t t')
	| TRec (x, s'), t ->
	    r_subtype (TypSet.add (s,t) rel) env (typ_subst x s s') t
	| _ -> s = t

  (* assumes fs1 and fs2 are ordered 
     fs1 <: fs2 if fs1 has everything fs2 does, and maybe more *)
  and r_subtype_fields rel env fs1 fs2 = match fs1, fs2 with
    | [], [] -> true
    | [], _ -> false (* fs1 is missing some things fs2 has *)
    | _, [] -> true (* can have many extra fields, doesn't matter *)
    | (x, s) :: fs1', (y, t) :: fs2' ->
        let cmp = String.compare x y in
          if cmp = 0 then r_subtype rel env s t && r_subtype_fields rel env fs1' fs2'
            (* if cmp < 0, x is an extra field, so just move on *)
          else (if cmp < 0 then 
                  (printf "%s is extra field\n" x; 
                   r_subtype_fields rel env fs1' fs2)
                    (* otherwise, y is a field that x does not have *)
                else (printf "lhs doesnt have %s\n" y; false))
            
  and r_subtype_star rel env fso fs_star other_typ = match fso, fs_star with
    | [], [] -> true
    (* extra things need to be r_subtypes of other_typ *)
    | (x, s) :: fs1', [] -> r_subtype rel env s other_typ && 
	r_subtype_star rel env fs1' fs_star other_typ
    (* named things exist in the ObjStar that aren't in the object *)
    | [], _ -> false
    (* otherwise, same as normal objects *)
    | (x, s) :: fs1', (y, t) :: fs2' ->
	(printf "%s %s\n" x y;
	let cmp = String.compare x y in
	  if cmp = 0 then r_subtype rel env s t && 
	    r_subtype_star rel env fs1' fs2' other_typ
          else (if cmp < 0 then 
                  (printf "%s is extra field (star)\n" x; 
		   r_subtype rel env s other_typ &&
                     (r_subtype_star rel env fs1' fs_star other_typ))
                else (printf "lhs doesnt have %s, checked v %s\n" y x; false)))

  and r_subtypes rel env (ss : typ list) (ts : typ list) : bool = 
    try List.for_all2 (r_subtype rel env) ss ts
    with Invalid_argument _ -> false (* unequal lengths *)

  let rec subtype env s t = r_subtype TypSet.empty env s t
  let rec subtypes env ss ts = r_subtypes TypSet.empty env ss ts

  let typ_union cs s t = match subtype cs s t, subtype cs t s with
      true, true -> s (* t = s *)
    | true, false -> t (* s <: t *)
    | false, true -> s (* t <: s *)
    | false, false -> TUnion (s, t)

  let rec normalize_typ env typ = 
    match typ with 
      | TUnion (s, t) -> 
          typ_union env (normalize_typ env s) (normalize_typ env t)
      | TObject fs ->
          let fs = List.fast_sort cmp_props fs in
            TObject (map (second2 (normalize_typ env)) fs)
      | TObjStar (fs, cname, other_typ) ->
	  let fs = List.fast_sort cmp_props fs in
	    if not (IdMap.mem cname env.classes) then
		raise (Not_wf_typ (cname ^ " is not a type constructor"))
	    else
	      let other_typ = normalize_typ env other_typ in
		TObjStar (fs, cname, other_typ)
      | TConstr ("Array", [t]) -> TConstr ("Array", [normalize_typ env t])
      | TConstr ("Array", (t:_)) -> 
          raise (Not_wf_typ ("Array only takes one argument"))
      | TConstr (constr, []) ->
          if IdMap.mem constr env.classes then typ
          else 
            begin
              raise (Not_wf_typ (constr ^ " is not a type constructor"))
            end
      | TConstr (constr, _) ->
          raise (Not_wf_typ (constr ^ " does not take arguments"))
      | TArrow (this, args, result) ->
          TArrow (normalize_typ env this, map (normalize_typ env) args,
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
	  let t' = normalize_typ (bind_typ_id x typ env) t in
	    TRec (x, t')
	      
  let check_typ p env t = 
    try
      match t with
        | TConstr ("Array", [tarr]) -> 
            if subtype env Typedjs_types.typ_undef tarr 
            then raise (Typ_error (
                          p, "array type can't be supertype of undefined"))
            else normalize_typ env t
        | _ -> normalize_typ env t
    with Not_wf_typ s -> raise (Typ_error (p, s))

  let basic_static env (typ : typ) (rt : RT.t) : typ = match rt with
    | RT.Num -> typ_union env typ_num typ
    | RT.Str -> typ_union env typ_str typ
    | RT.Bool -> typ_union env typ_bool typ
    | RT.Function -> typ_union env TField typ
    | RT.Object -> typ_union env TField typ
    | RT.Undefined -> typ_union env typ_undef typ

  let basic_static2 env (typ : typ) (rt : RT.t) : typ = match rt with
    | RT.Num -> typ_union env typ_num typ
    | RT.Str -> typ_union env typ_str typ
    | RT.Bool -> typ_union env typ_bool typ
    | RT.Function -> typ_union env (TObject []) typ
    | RT.Object -> typ_union env (TObject []) typ
    | RT.Undefined -> typ_union env typ_undef typ

  let rec static cs (rt : RTSet.t) (typ : typ) : typ = match typ with
    | TBot -> TBot (* might change if we allow arbitrary casts *)
    | TArrow _ -> if RTSet.mem RT.Function rt then typ else TBot
    | TConstr ("Str", []) -> if RTSet.mem RT.Str rt then typ else TBot
    | TConstr ("RegExp", []) -> if RTSet.mem RT.Object rt then typ else TBot
    | TConstr ("Num", []) -> if RTSet.mem RT.Num rt then typ else TBot
    | TConstr ("Int", []) -> if RTSet.mem RT.Num rt then typ else TBot
    | TConstr ("Bool", []) -> if RTSet.mem RT.Bool rt then typ else TBot
    | TConstr ("Undef", []) -> 
        if RTSet.mem RT.Undefined rt then typ else TBot
          (* any other app will be an object from a constructor *)
    | TConstr _ -> if RTSet.mem RT.Object rt then typ else TBot
    | TObject _ -> if RTSet.mem RT.Object rt then typ else TBot
    | TObjStar _ -> if RTSet.mem RT.Object rt then typ else TBot
    | TRef t -> TRef t
    | TSource t -> TSource t
    | TSink t -> TSink t
    | TUnion (s, t) -> typ_union cs (static cs rt s) (static cs rt t)
    | TForall _ -> typ
    | TRec (s, t) -> typ
    | TField -> List.fold_left (basic_static cs) TBot (RTSetExt.to_list rt)
    | TTop -> 
        if RTSet.equal rt Typedjs_lattice.rtany then
          TTop
        else (* TODO: no arrow type that is the supertype of all arrows *)
          List.fold_left (basic_static2 cs) TBot (RTSetExt.to_list rt)
    | TId _ -> typ


  let new_root_class env class_name = 
    if IdMap.mem class_name env.classes then
      raise (Invalid_argument ("class already exists: " ^ class_name))
    else 
      let c = IdMap.add class_name 
        { fields = IdMap.empty; sup = None } env.classes in
        { env with
            classes = c
        }

  let new_subclass env sub_name sup_name =
    { env with
        classes = IdMap.add sub_name
        { fields = IdMap.empty; sup = Some sup_name } env.classes;
        subclasses = IdMap.add sub_name sup_name env.subclasses }

  let add_method c_name m_name m_typ env =
    let ci = IdMap.find c_name env.classes in
      if IdMap.mem m_name ci.fields then
        raise (Invalid_argument ("method already exists: " ^ m_name))
      else 
        let ci' = { ci with fields = IdMap.add m_name m_typ ci.fields } in
          { env with classes = IdMap.add c_name ci' env.classes }

  let rec set_global_object env cname =
    let ci = IdMap.find cname env.classes in
    let fs = IdMapExt.to_list ci.fields in
    let add_field env (x, t) = bind_id x t env in
    let env = List.fold_left add_field env fs in
      match ci.sup with
        | None -> env
        | Some cname' -> set_global_object env cname'

  let rec bind_typ env typ : env * typ = match typ with
    | TForall (x, s, t) -> bind_typ (bind_typ_id x s env) t
    | typ -> (env, typ)


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



let rec add_methods (lst : (id * typ) list) (class_name : id) (env : Env.env) = 
  match lst with
      [] -> env
    | (method_name, method_typ) :: rest ->
        add_methods rest class_name
          (Env.add_method class_name method_name method_typ env)

let rec add_classes (lst : env_decl list) (env : Env.env) = match lst with
  | [] -> env
  | EnvClass (c_name, Some p_name, _) :: rest ->
      add_classes rest (Env.new_subclass env c_name p_name)
  | EnvClass (cname, None, _) :: rest ->
      add_classes rest (Env.new_root_class env cname)
  | _ :: rest -> add_classes rest env

(* [mk_env'] ensures that a type declaration is  well-formed and well-kinded.
   For these checks, it needs the existing environment. *)
let rec mk_env' (lst : env_decl list) (env : Env.env) : Env.env =  
  match lst with
    | [] -> env
    | EnvBind (x, typ) :: rest ->
        if IdMap.mem x env.Env.id_typs then
          raise (Not_wf_typ (x ^ " is already bound in the environment"))
        else
          mk_env' rest (Env.bind_id x typ env)
    | EnvClass (class_name, proto, methods) :: rest ->
        let env = try match proto with
          | None -> add_methods methods class_name env
          | Some proto_name ->
              add_methods methods class_name env
        with Not_wf_typ s ->
          raise (Not_wf_typ ("error adding class " ^ class_name ^ "; " ^ s)) in
          (* TODO account for prototype *)
          mk_env' rest env

let extend_global_env env lst = mk_env' lst (add_classes lst env)

module L = Typedjs_lattice

let df_func_of_typ (t : typ) : L.av list -> L.av = match t with
  | TArrow (_, _, r_typ) ->
      let r_av = L.ASet (L.rt_of_typ r_typ) in
        (fun _ -> r_av)
  | TForall (x, r_typ, TArrow (_, _, TId y)) when x = y ->
      let r_av = L.ASet (L.rt_of_typ r_typ) in
        (fun _ -> r_av)
  | _ -> (fun _ -> L.any)

let cf_env_of_tc_env tc_env = 
  let fn x typ cf_env = L.bind x (L.runtime typ) cf_env in
    IdMap.fold fn (Env.id_env tc_env) L.empty_env

let operator_env_of_tc_env tc_env =
  let fn x t env = IdMap.add x (df_func_of_typ t) env in
    IdMap.fold fn (Env.id_env tc_env) IdMap.empty
  

let apply_subst subst typ = IdMap.fold typ_subst subst typ

 (* TODO: occurs check *)
let rec unify subst s t : typ IdMap.t = match s, t with
  | TId x, TId y -> 
      if x = y then subst 
      else failwith ("cannot unify bound variables " ^ x ^ " and " ^ y)
  | TId x, t -> 
      if IdMap.mem x subst then
        begin
          let s = IdMap.find x subst in
            IdMap.add x (apply_subst subst (TUnion (s, t))) subst
        end
      else
        IdMap.add x (apply_subst subst t) subst
  | s, TId y -> unify subst (TId y) s

  | TConstr (c1, ss), TConstr (c2, ts) ->
      if c1 = c2 then
        List.fold_left2 unify subst ss ts
      else 
        failwith ("cannot unify constructors " ^ c1 ^ " and " ^ c2)


  | TUnion (s1, s2), TUnion (t1, t2) -> unify (unify subst s1 t1) s2 t2

  | TArrow (s1, s2s, s3), TArrow (t1, t2s, t3) ->
      List.fold_left2 unify subst (s1 :: s3 :: s2s) (t1 :: t3 :: t2s)
  | TObject fs1, TObject fs2 ->
      let f subst (x, s) (y, t) = 
        if x = y then unify subst s t
        else failwith "cannot unify objects with distinct field names" in
      List.fold_left2 f subst fs1 fs2
  | TRef s, TRef t -> unify subst s t
  | TSource s, TSource t -> unify subst s t
  | TSink s, TSink t -> unify subst s t
  | TTop, TTop -> subst
  | TBot, TBot -> subst
  | TForall _, TForall _ -> failwith "cannot unify quantified types"
  | _ -> failwith "unification failure"


let unify_typ (s : typ) (t : typ) : typ IdMap.t = 
  unify IdMap.empty s t

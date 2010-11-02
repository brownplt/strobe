open Prelude
open Typedjs_syntax
open Typedjs_types

exception Not_wf_typ of string

let rec fill n a l = if n <= 0 then l else fill (n-1) a (List.append l [a])

module Env = struct

  module TypPairOrderedType = struct
    type t = typ * typ
    let compare = Pervasives.compare
  end
    
  module TypPairSet = Set.Make (TypPairOrderedType)

  type class_info = {
    fields : typ IdMap.t;
    sup : constr option
  }

  type env = {
    global : typ;
    id_typs : typ IdMap.t; 
    lbl_typs : typ IdMap.t;
    classes : class_info IdMap.t;
    subclasses : id IdMap.t; (* direct subclasses *)
    typ_ids: typ IdMap.t; (* bounded type variables *)
    synonyms : typ IdMap.t
  }


  let empty_env = { 
    global = TObject [];
    id_typs = IdMap.empty;
    lbl_typs = IdMap.empty;
    classes = IdMap.empty;
    subclasses = IdMap.empty;
    typ_ids = IdMap.empty;
    synonyms = IdMap.empty;
  }

  let bind_global t env = { env with global = t }

  let lookup_global env = env.global

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

  let rec pick_typs ids typ_map = match ids with
    | [] -> IdMap.empty
    | x :: rest -> 
      if IdMap.mem x typ_map then
        IdMap.add x (IdMap.find x typ_map) (pick_typs rest typ_map)
      else
        pick_typs rest typ_map
        
  let clear_labels env = 
    { env with 
      lbl_typs = pick_typs [ "%return"; "%break"; "%continue" ] env.lbl_typs
    }

  let dom env = IdSetExt.from_list (IdMapExt.keys env.id_typs)

  let class_fields env constr = (IdMap.find constr env.classes).fields

  let class_sup env constr = (IdMap.find constr env.classes).sup

  let cmp_props (k1, _) (k2, _) = match String.compare k1 k2 with
    | 0 -> raise (Not_wf_typ ("the field " ^ k1 ^ " is repeated"))
    | n -> n

  let rec r_subtype rel env s t =
    let st = r_subtype rel env in
      if (TypPairSet.mem (s,t) rel) then true
      else match s, t with
	| TId x, TId y -> x = y
	| TId x, t -> begin try
	    let s = IdMap.find x env.typ_ids in
	      r_subtype rel env s t
          with Not_found -> failwith ("Unbound id (in subtype, shouldn't happen): " ^ x)
          end
	| s, TRec (x, t') -> 
	    r_subtype (TypPairSet.add (s,t) rel) env s (Typedjs_syntax.Typ.typ_subst x t t')
	| TRec (x, s'), t ->
	    r_subtype (TypPairSet.add (s,t) rel) env (Typedjs_syntax.Typ.typ_subst x s s') t
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
            
            (** We can fill args2 with undefineds (up to the length of
            args1), and then check *)

        | TArrow (this_t1, args1, r1), TArrow (this_t2, args2, r2) ->
            let args2 = 
              fill (List.length args1 - List.length args2) 
                typ_undef args2 in
              r_subtypes rel env args2 args1 && r_subtype rel env r1 r2 && 
                st this_t1 this_t2 && st this_t2 this_t1
        | TConstr (c_name, []), TObject fs2 ->
            (* Classes can be turned into objects. However, this drops
               all fields in the prototype. *)
            let fs1 = IdMapExt.to_list (IdMap.find c_name env.classes).fields in
            let fs1 = List.rev fs1 in
              r_subtype_fields rel env fs1 fs2
	| TObjStar (fs1, cnames1, other_typ1, code1), 
	    TObjStar (fs2, cnames2, other_typ2, code2) ->
	    r_subtype_fields rel env fs1 fs2 &&
	      cnames1 = cnames2 &&
	      st other_typ1 other_typ2 &&
              st code1 code2
        | TSource s, TSource t -> st s t
        | TSink s, TSink t -> st t s
        | TRef s, TSource t -> st s t
        | TRef s, TSink t -> st t s
        | TConstr (constr, []), TField ->
            List.mem constr [ "Num"; "Int"; "Str"; "Undef"; "Bool" ]
	| _, TTop -> true
	| TBot, _ -> true
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
                  r_subtype_fields rel env fs1' fs2
                    (* otherwise, y is a field that x does not have *)
                else false)
            
  and r_subtypes rel env (ss : typ list) (ts : typ list) : bool = 
    try List.for_all2 (r_subtype rel env) ss ts
    with Invalid_argument _ -> false (* unequal lengths *)

  let rec subtype env s t = r_subtype TypPairSet.empty env s t
  let rec subtypes env ss ts = r_subtypes TypPairSet.empty env ss ts

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
      | TObjStar (fs, cnames, other_typ, code) ->
	  let fs = List.fast_sort cmp_props fs in
          let fs = map (second2 (normalize_typ env)) fs in
	    if List.exists (fun cname -> not (IdMap.mem cname env.classes)) cnames then
		raise (Not_wf_typ ("Non type constructor in objstar"))
	    else
	      let other_typ = normalize_typ env other_typ in
              let code = normalize_typ env code in
		TObjStar (fs, cnames, other_typ, code)
      | TConstr ("Array", [t]) -> TConstr ("Array", [normalize_typ env t])
      | TConstr ("Array", (t:_)) ->
          raise (Not_wf_typ ("Array only takes one argument"))
      | TConstr (constr, []) ->
          if IdMap.mem constr env.classes then 
            typ
          else
            raise (Not_wf_typ (constr ^ " is not a type constructor"))
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
          else if IdMap.mem x env.synonyms then
            normalize_typ env (IdMap.find x env.synonyms)
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

  let has_obj rtset = RTSet.exists (fun rt -> match rt with 
                                   | RT.Object _ -> true
                                   | _ -> false) rtset

  let basic_static env (typ : typ) (rt : RT.t) : typ = match rt with
    | RT.Num -> typ_union env typ_num typ
    | RT.Str -> typ_union env typ_str typ
    | RT.Bool -> typ_union env typ_bool typ
    | RT.Function -> typ_union env TField typ
    | RT.Object _ -> typ_union env TField typ
    | RT.ConstrObj _ -> typ_union env TField typ
    | RT.Undefined -> typ_union env typ_undef typ

  let basic_static2 env (typ : typ) (rt : RT.t) : typ = match rt with
    | RT.Num -> typ_union env typ_num typ
    | RT.Str -> typ_union env typ_str typ
    | RT.Bool -> typ_union env typ_bool typ
    | RT.Function -> typ_union env (TObject []) typ
    | RT.Object _ -> typ_union env (TObject []) typ
    | RT.ConstrObj _ -> typ_union env (TObject []) typ (* parameters unknown *)
    | RT.Undefined -> typ_union env typ_undef typ

(* maybe_falsy takes refs (fields) and indicates if they could
*possibly* be falsy values *)
  let maybe_falsy env typ = 
    match typ with
      | TRef typ ->
          List.exists (fun t -> subtype env t typ)
            [TConstr ("Bool", []);
             TConstr ("Int", []);
             TConstr ("Num", []);
             TConstr ("Str", []);
             TConstr ("Undef", []);
             TConstr ("Null", [])]
      | _ -> true

  let rec static cs (rt : RTSet.t) (typ : typ) : typ = match typ with
    | TBot -> TBot (* might change if we allow arbitrary casts *)
    | TArrow _ -> if RTSet.mem RT.Function rt then typ else TBot
    | TConstr ("Str", []) -> if RTSet.mem RT.Str rt then typ else TBot
    | TConstr ("RegExp", []) -> if has_obj rt then typ else TBot
    | TConstr ("Num", []) -> if RTSet.mem RT.Num rt then typ else TBot
    | TConstr ("Int", []) -> if RTSet.mem RT.Num rt then typ else TBot
    | TConstr ("Bool", []) -> if RTSet.mem RT.Bool rt then typ else TBot
    | TConstr ("Undef", []) -> 
        if RTSet.mem RT.Undefined rt then typ else TBot
          (* any other app will be an object from a constructor *)
    | TConstr (constr_name, _) -> 
        (* If there exists a non-falsy value in the constructor that
        the flow analysis marked as falsy, then this constructor is
        not part of the type *)
        if (RTSet.exists 
              (fun rt' -> 
                 match rt' with
                   | RT.Object ([fld]) -> 
                       (try let fld = 
                          IdMap.find fld (class_fields cs constr_name)
                        in not (maybe_falsy cs fld)
                       with Not_found -> false)
                   | _ -> false) rt) 
        then
          TBot 
        else
          (* Two conditions due to ordering in the lattice of abstract values *)
          if RTSet.mem (RT.ConstrObj constr_name) rt || has_obj rt then
            typ
          else
            TBot
    | TObject _ -> if has_obj rt then typ else TBot
    | TObjStar (_, _, _, code) -> 
        (* If it is labeled as a function and only a function, it is
        safe to treat it as just the code part.  If it is an object at
        all, keep the whole obj* type *)
        begin match (has_obj rt, RTSet.mem RT.Function rt) with
          | (true, _) -> typ
          | (false, true) -> code
          | (false, false) -> TBot
        end
    | TRef t -> TRef t
    | TSource t -> TSource t
    | TSink t -> TSink t
    | TUnion (s, t) -> typ_union cs (static cs rt s) (static cs rt t)
    | TForall _ -> typ
    | TRec (x, t) -> static cs rt (Typedjs_syntax.Typ.typ_subst x typ t)
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

  let get_global_object env = env.global


  let rec set_global_object' env cname =
    let ci = IdMap.find cname env.classes in
    let fs = IdMapExt.to_list ci.fields in
    let add_field env (x, t) = bind_id x t env in
    let env = List.fold_left add_field env fs in
      match ci.sup with
        | None -> env
        | Some cname' -> set_global_object' env cname'

  let set_global_object env cname =
    set_global_object' (bind_global (TConstr (cname, [])) env) cname

  let rec bind_typ env typ : env * typ = match typ with
    | TForall (x, s, t) -> bind_typ (bind_typ_id x s env) t
    | typ -> (env, typ)

      
  module Pretty = struct
      
    open Format
    open FormatExt
    open Typedjs_syntax.Pretty
      
    let empty : printer = fun _ -> ()
      
    let p_id_bind x t fmt = match t with
      | TRef s -> vert [ horz [ text x; text ":"; p_typ s ]; fmt ]
      | _ -> vert [ horz [ text "val"; text x; text ":"; p_typ t ]; fmt ]

    let p_field x t fmt =
      vert [ horz [ text x; text ":"; p_typ t; text "," ]; fmt ]

    (* TODO: Does not print "checked". *)
    let p_class_def c_name c_info fmt =
      let class_line = match c_info.sup with
        | None -> horz [ text "class"; text c_name ]
        | Some sup ->
          horz [ text "class"; text c_name; text "prototype"; text sup ] in
      vert [ class_line; braces (IdMap.fold p_field c_info.fields empty); fmt ]

    (* TODO: warn if lbl_typs or typ_ids are non-empty *)
    let p_env (env : env) = 
      let ids = IdMap.fold p_id_bind env.id_typs empty in
      let classes =  IdMap.fold p_class_def env.classes empty in
      vert [ ids; classes ]

  end

  let rec diff final_env init_env =
    { 
      global = final_env.global;
      id_typs = IdMapExt.diff final_env.id_typs init_env.id_typs;
      lbl_typs = IdMapExt.diff final_env.lbl_typs init_env.lbl_typs;
      classes = IdMapExt.diff final_env.classes init_env.classes;
      subclasses = IdMapExt.diff final_env.subclasses init_env.subclasses;
      typ_ids = IdMapExt.diff final_env.typ_ids init_env.typ_ids;
      synonyms = IdMapExt.diff final_env.synonyms init_env.synonyms; 
    }

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
    | EnvTypSyn (x, typ) :: rest -> 
      let typ' = Env.normalize_typ env typ in
      if IdMap.mem x env.Env.synonyms then
        raise (Not_wf_typ (sprintf "%s is already used as a type-synonym" x))
      else
        mk_env' rest
          ({ env with Env.synonyms = IdMap.add x typ' env.Env.synonyms })

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
  
let apply_subst subst typ = IdMap.fold Typedjs_syntax.Typ.typ_subst subst typ

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

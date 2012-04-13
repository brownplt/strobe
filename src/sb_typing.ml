open Prelude
open Typedjs_syntax
open TypImpl
open Typedjs_env
open Format
open Typedjs_tc_util

let consumed_owned_vars  = ref IdSet.empty

let array_idx_pat = 
  P.parse Lexing.dummy_pos
    "(([0-9])*|(\"+Infinity\"|(\"-Infinity\"|\"NaN\")))"


let mk_array_typ p env elt_typ =
  TApp (TId "Array", [elt_typ])

let is_flows_enabled = ref true

let disable_flows () = is_flows_enabled := false

let rec skip n l = if n == 0 then l else (skip (n-1) (List.tl l))
let rec fill n a l = if n <= 0 then l else fill (n-1) a (List.append l [a])

let un_null t = match t with
  | TUnion (n, TPrim "Undef", t')
  | TUnion (n, t', TPrim "Undef")
  | TUnion (n, TPrim "Null", t')
  | TUnion (n, t', TPrim "Null") -> apply_name n t'
  | _ -> t

let check_kind p env typ : typ =
  try
    match kind_check env typ with
      | KStar -> typ
      | k ->
        raise (Typ_error  (p, TypKind((fun t k -> sprintf "type %s has kind %s; expected *"
          (string_of_typ t) (string_of_kind k)), typ, k)))
  with
    | Sb_kinding.Kind_error msg ->
      Printf.eprintf "Couldn't check type for %s\n" (string_of_typ typ);
        raise (Sb_kinding.Kind_error (string_of_position p ^ ": " ^ msg))
  
let expose_simpl_typ env typ = expose env (simpl_typ env typ)

let combine n m = if n = None then m else n
let extract_ref msg p env t = 
  let rec helper t n = match expose_simpl_typ env t with
    | TPrim _  -> None
    | TRegex _ -> Some t
    | TRef _ as t -> Some (apply_name n t)
    | TSource _ as t -> Some (apply_name n t)
    | TSink _ as t -> Some (apply_name n t)
    | TRec(n', _, _) as t -> helper (simpl_typ env t) (combine n' n)
    | TThis t -> helper t n
    | TUnion (n', t1, t2) -> 
      let r1 = helper t1 (combine n' n) in
      let r2 = helper t2 (combine n' n) in
      (match r1, r2 with
      | Some r, None
      | None, Some r
      | Some r, Some (TRegex _)
      | Some (TRegex _), Some r -> Some (apply_name (combine n' n) r)
      | _ -> None) 
    | TIntersect (n', t1, t2) -> 
      let r1 = helper t1 (combine n' n) in
      let r2 = helper t2 (combine n' n) in
      (match r1, r2 with
      | Some r, None
      | None, Some r
      | Some r, Some (TRegex _)
      | Some (TRegex _), Some r -> Some (apply_name (combine n' n) r)
      | _ -> None)
    | TForall _ -> Some (apply_name n t) (* BSL : This seems incomplete; extract_ref won't descend under a Forall *)
    | _ -> (* (printf "%s: Got to %s\n" msg (string_of_typ t)); *) None in
  match helper t None with
  | Some t -> t
  | None -> raise (Typ_error (p, StringTyp((fun s t -> sprintf "%s: Ambiguous ref type for type %s"
    s (string_of_typ t)), msg, t)))

let extract_arrow env t = 
  let rec helper t = match expose_simpl_typ env t with
    | TArrow _ as t -> Some t
    | TUnion (_, t1, t2) -> 
      let r1 = helper t1 in
      let r2 = helper t2 in
      (match r1, r2 with
      | Some r, None
      | None, Some r -> Some r
      | _ -> None) 
    | TForall _ -> Some t (* BSL : This seems incomplete; extract_arrow won't descend under a Forall *)
    | _ -> None in
  match helper t with
  | Some t -> t
  | None -> raise (Typ_error ((Lexing.dummy_pos, Lexing.dummy_pos), FixedString "Ambiguous arrow type for Func"))


let simpl_print e = match e with
  | EConst(p, _) -> "EConst " ^ (string_of_position p)
  | EBot(p) -> "EBot " ^ (string_of_position p)
  | EAssertTyp(p, _, _) -> "EAssertTyp " ^ (string_of_position p)
  | EArray(p, _) -> "EArray " ^ (string_of_position p)
  | EObject(p, _) -> "EObject " ^ (string_of_position p)
  | EId(p, x) -> "EId " ^ x ^ " " ^ (string_of_position p)
  | EBracket(p, _, _) -> "EBracket " ^ (string_of_position p)
  | EUpdate(p, _, _, _) -> "EUpdate " ^ (string_of_position p)
  | EPrefixOp(p, _, _) -> "EPrefixOp " ^ (string_of_position p)
  | EInfixOp(p, _, _, _) -> "EInfixOp " ^ (string_of_position p)
  | EIf(p, _, _, _) -> "EIf " ^ (string_of_position p)
  | EApp(p, _, _) -> "EApp " ^ (string_of_position p)
  | EFunc(p, _, _, _) -> "EFunc " ^ (string_of_position p)
  | ELet(p, _, _, _) -> "ELet " ^ (string_of_position p)
  | ERec(p, _, _) -> "ERec " ^ (string_of_position p)
  | ESeq(p, _, _) -> "ESeq " ^ (string_of_position p)
  | ELabel(p, _, _) -> "ELabel " ^ (string_of_position p)
  | EBreak(p, _, _) -> "EBreak " ^ (string_of_position p)
  | ETryCatch(p, _, _, _) -> "ETryCatch " ^ (string_of_position p)
  | ETryFinally(p, _, _) -> "ETryFinally " ^ (string_of_position p)
  | EThrow(p, _) -> "EThrow " ^ (string_of_position p)
  | ETypecast(p, _, _) -> "ETypecast " ^ (string_of_position p)
  | ERef(p, _, _) -> "ERef " ^ (string_of_position p)
  | EDeref(p, _) -> "EDeref " ^ (string_of_position p)
  | ESetRef(p, _, _) -> "ESetRef " ^ (string_of_position p)
  | ESubsumption(p, _, _) -> "ESubsumption " ^ (string_of_position p)
  | EDowncast(p, _, _) -> "EDowncast " ^ (string_of_position p)
  | ETypAbs(p, _, _, _) -> "ETypAbs " ^ (string_of_position p)
  | ETypApp(p, _, _) -> "ETypApp " ^ (string_of_position p)
  | ECheat(p, _, _) -> "ECheat " ^ (string_of_position p)
  | EParen(p, _) -> "EParen " ^ (string_of_position p)


let rec usesThis exp = match exp with
  | EConst (p, c) -> false
  | EBot p -> false
  | EAssertTyp (p, t, e) -> usesThis e
  | EArray (p, es) -> List.exists usesThis es
  | EObject (p, flds) -> List.exists usesThis (map snd flds)
  | EId (p, x) -> x = "this"
  | EBracket (p, e1, e2) -> usesThis e1 || usesThis e2
  | EUpdate (p, e1, e2, e3) -> usesThis e1 || usesThis e2 || usesThis e3
  | EPrefixOp (p, op, e) -> usesThis e
  | EInfixOp (p, op, e1, e2) -> usesThis e1 || usesThis e2
  | EIf (p, e1, e2, e3) -> usesThis e1 || usesThis e2 || usesThis e3
  | EApp (p, e, es) -> List.exists usesThis (e::es)
  | EFunc (p, xs, fi, e) -> usesThis e
  | ELet (p, x, e1, e2) -> usesThis e1 || usesThis e2
  | ERec (p, binds, body) -> usesThis body
  | ESeq (p, e1, e2) -> usesThis e1 || usesThis e2
  | ELabel (p, x,  e) -> usesThis e
  | EBreak (p, x, e) -> usesThis e
  | ETryCatch (p, e1, x, e2) -> usesThis e1 || usesThis e2
  | ETryFinally (p, e1, e2) -> usesThis e1 || usesThis e2
  | EThrow (p, e) -> usesThis e
  | ETypecast (p, s, e) -> usesThis e
  | ERef (p, k, e) -> usesThis e
  | EDeref (p, e) -> usesThis e
  | ESetRef (p, e1, e2) -> usesThis e1 || usesThis e2
  | ESubsumption (p, t, e) -> usesThis e
  | EDowncast (p, t, e) -> usesThis e
  | ETypAbs (p, x, t, e) -> usesThis e
  | ETypApp (p, e, t) -> usesThis e
  | ECheat (p, t, e) -> usesThis e
  | EParen (p, e) -> usesThis e

let depth = ref 0
let trace (msg : string) (thunk : exp -> 'a) (exp : exp)  = thunk exp
    (* Printf.eprintf "%s-->%s %s\n" (String.make (!depth) ' ') msg (simpl_print exp); *)
    (* depth := !depth + 1; *)
    (* try *)
    (*   let ret = thunk exp in *)
    (*   depth := !depth - 1; *)
    (*   Printf.eprintf "%s<--%s %s\n" (String.make (!depth) ' ') msg (simpl_print exp); *)
    (*   ret *)
    (* with e -> *)
    (*   depth := !depth - 1; *)
    (*   Printf.eprintf "%s<X-%s %s\n" (String.make (!depth) ' ') msg (simpl_print exp); *)
    (*   raise e *)

let rec check (env : env) (default_typ : typ option) (exp : exp) (typ : typ) : unit =
  try trace "Check" (fun exp -> check' env default_typ exp typ) exp
  (* typ_mismatch normally records the error and proceeds. If we're in a
     [with_typ_exns] context, it will re-raise the error. *)
  with Typ_error (p, s) -> typ_mismatch p s
    
and check' (env : env) (default_typ : typ option) (exp : exp) (typ : typ) : unit = 
  (* Printf.eprintf "Check': checking %s : %s\n" (string_of_exp exp) (string_of_typ typ); *)
match exp with
  | ELabel (p, lbl, e) ->
    (* Printf.eprintf "Check': Binding label %s in expression %s\n" lbl (string_of_exp e); *)
    let s = synth (bind_lbl lbl typ env) default_typ e in
    if not (subtype env s (expose env typ)) then
      typ_mismatch p (TypTyp((fun t1 t2 -> sprintf "label has type %s, but body returns %s"
        (string_of_typ t1) (string_of_typ t2)), typ, s))
  | ELet (_, x, e1, e2) -> 
    check (bind_id x (synth env default_typ e1) env) default_typ e2 typ
  | ECheat (p, t, (EFunc(pf, args, func_info, body) as f)) -> 
    let simpl_t = expose_simpl_typ env t in
    check env default_typ f simpl_t
  | EFunc (p, args, func_info, body) -> 
    let misowned_vars = IdSet.inter !consumed_owned_vars 
      func_info.func_owned in
    consumed_owned_vars := IdSet.union !consumed_owned_vars
      func_info.func_owned;
    let owned_vars = IdSet.diff func_info.func_owned misowned_vars in
    begin match bind_typ env (extract_arrow env (expose_simpl_typ env (check_kind p env (expose_simpl_typ env typ)))) with
      | (env, TArrow (arg_typs, None, result_typ)) ->
        if not (List.length arg_typs = List.length args) then
          typ_mismatch p
            (NumNum(sprintf "given %d argument names, but %d argument types", (List.length args), (List.length arg_typs)))
        else
          let bind_arg env x t = bind_id x t env in
          let env = List.fold_left2 bind_arg env args arg_typs in
          let env = clear_labels env in
          let body = 
            if !is_flows_enabled then
              Sb_semicfa.semicfa owned_vars env body 
            else 
              body in
          (* printf "Owned = %s\n" (FormatExt.to_string
            (IdSetExt.p_set FormatExt.text) func_info.func_owned);
          printf "Rewritten to:\n%s\n\n\n" (string_of_exp body);  *)
          check env default_typ body result_typ
      | (env, TArrow (arg_typs, Some vararg_typ, result_typ)) ->
        let bind_arg env x t = bind_id x t env in
        let bind_vararg env x = bind_arg env x vararg_typ in
        let (req_args, var_args) = ListExt.split_at (List.length arg_typs) args in
        let env = List.fold_left2 bind_arg env req_args arg_typs in
        let env = List.fold_left bind_vararg env var_args in
          let body = 
            if !is_flows_enabled then
              Sb_semicfa.semicfa owned_vars env body 
            else 
              body in
          (* printf "Owned = %s\n" (FormatExt.to_string
            (IdSetExt.p_set FormatExt.text) func_info.func_owned);
          printf "Rewritten to:\n%s\n\n\n" (string_of_exp body);  *)
          check env default_typ body result_typ
      | _, t -> 
        raise (Typ_error (p, Typ((fun t -> sprintf "invalid type annotation on a function: %s" (string_of_typ t)), t)))
    end
  | ERef (p, ref_kind, e) -> 
    begin match ref_kind, extract_ref "ERef" p env (check_kind p env (expose_simpl_typ env typ)) with
    | SourceCell, TSource (_, t)
    | SinkCell, TSink (_, t)
    | RefCell, TRef (_, t) -> check env default_typ e t
    | _, t  -> typ_mismatch p (* TODO(arjun): error msg *)
             (TypTyp((fun t1 t2 -> sprintf "!!expected %s, got %s"
               (string_of_typ t1) (string_of_typ t2)), t, (synth env default_typ e)))
    end
  | EArray (p, es) ->
      let expect_elt_typ = 
        simpl_lookup p (tid_env env)
          (un_null (expose_simpl_typ env typ)) array_idx_pat in
      let expect_array_typ = mk_array_typ p env expect_elt_typ in
      (if not (subtype env (TRef (None, typ)) expect_array_typ) then
         typ_mismatch p 
           (TypTyp((fun t1 t2 -> sprintf "check: expected Array<%s>, got %s" (string_of_typ t1) (string_of_typ t2)), expect_elt_typ, (TRef (None, typ)))));
      List.iter (fun e -> check env default_typ e expect_elt_typ) es 
  | EParen (_, e) -> check env default_typ e typ
  | EObject (p, fields) -> begin match expose_simpl_typ env typ with
    | TObject _ as t -> 
      let absPat = P.negate (List.fold_left P.union P.empty 
                               (List.map (fun (f, _) -> P.singleton f) fields)) in
      let newObjTyp = TObject (mk_obj_typ 
                                 (List.map (fun (fieldName, fieldExp) -> 
                                   let fieldTyp = (inherits p env t (P.singleton fieldName)) in
                                   (* Printf.eprintf "Check' EObject Checking field %s\n" fieldName; *)
                                   check env default_typ fieldExp fieldTyp;
                                   (P.singleton fieldName, Present, fieldTyp)) fields)
                                 absPat) in
      if not (subtype env newObjTyp typ) then
        typ_mismatch (Exp.pos exp)
          (TypTyp((fun t1 t2 -> sprintf "**expected %s, got %s" (string_of_typ t1) (string_of_typ t2)), 
                  typ, newObjTyp))
    | _ -> typ_mismatch p (Typ((fun t -> sprintf "expected TObject, got %s" (string_of_typ t)), typ))
  end
  | _ -> 
    (* Printf.eprintf "Check': Synthing type for expression\n"; *)
    let synth_typ = expose_simpl_typ env (synth env default_typ exp) in
    (* Printf.printf "Checking %s <?: %s\n" (string_of_typ synth_typ) (string_of_typ (expose_simpl_typ env typ)); *)
    if not (subtype env synth_typ (expose_simpl_typ env typ)) then begin
      (* Printf.printf "failed.\n"; *)
      typ_mismatch (Exp.pos exp)
        (TypTyp((fun t1 t2 -> sprintf "%%expected %s to have type %s, got %s" (string_of_exp exp) (string_of_typ t1) (string_of_typ t2)), (expose_simpl_typ env typ), synth_typ))
    end (* else *)
      (* Printf.printf "Checking finished.\n" *)

and synth (env : env) (default_typ : typ option) (exp : exp) : typ = 
  trace "Synth" (synth' env default_typ) exp
and synth' env default_typ exp : typ =
    (* Printf.eprintf "*Synthing type for %s\n" (string_of_exp exp); *)
match exp with
  (* TODO: Pure if-splitting rule; make more practical by integrating with
      flow typing. *)
  | EIf (p, EInfixOp (_, "hasfield",  EDeref (_, EId (_, obj)), (EId (_, fld))),
         true_part, false_part) ->
    begin match expose_simpl_typ env (lookup_id obj env), lookup_id fld env with
      | TRef (_, TObject ot), TRegex pat -> 
        let subtract (p, pres, t) =
          if P.is_overlapped p pat then (P.subtract p pat, pres, t) (* perf *)
          else (p, pres, t) in
        let false_typ = synth env default_typ false_part in
        let true_typ =
          let fld_typ = simpl_lookup p (tid_env env) (TObject ot) pat in
          let env = bind_typ_id "alpha" (TRegex pat) env in
          let env = bind_id fld (TId "alpha") env in
          let env = bind_id obj 
            (TObject (mk_obj_typ ((P.var "alpha", Present, fld_typ) ::
                                     map subtract (fields ot))
                      (absent_pat ot))) env in
          synth env default_typ true_part in
        typ_union env true_typ false_typ
      | s, t ->
        raise (Typ_error (p, TypTyp((fun t1 t2 -> sprintf "expected object and string types, got %s and %s"
          (string_of_typ t1) (string_of_typ t2)), s, t)))
    end
  | EConst (_, c) -> tc_const c
  | EBot _ -> TBot
  | EId (p, x) -> begin
    try 
      lookup_id x env
    with Not_found -> match default_typ with
    | None -> raise (Typ_error (p, String(sprintf "%s is not defined", x))) (* severe *)
    | Some t -> 
      Printf.eprintf "Warning: Unbound identifier %s at %s\n" x (string_of_position p);
      Printf.eprintf "Currently bound identifiers are:\n";
      IdMap.iter (fun id _ -> Printf.eprintf "%s, " id) (id_env env);
      Printf.eprintf "\n";
      t (* Should probably warn about undefined identifier here *)
  end
  | ELet (_, x, e1, e2) -> synth (bind_id x (synth env default_typ e1) env) default_typ e2
  | ESeq (_, e1, e2) -> begin match synth env default_typ e1 with
      TBot -> (* e1 will not return; no need to typecheck e2 *)
        TBot
      | _ -> synth env default_typ e2
  end
  | EArray (p, []) -> 
    raise (Typ_error (p, FixedString "synth: an empty array literal requires a type annotation"))
  | EArray (p, e::es) -> 
    let (t1, ts) = synth env default_typ e, map (synth env default_typ) es in
    let tarr = List.fold_right (typ_union env) ts t1 in
    begin match simpl_typ env (mk_array_typ p env tarr) with
      | TRef (_, t) -> t
      | _ -> failwith "synth: mk_array_typ did not produce a TRef"
    end
  (* Optimization to avoid reducing TArray<'a> *)
  | ERef (p1, RefCell, EArray (p2, e::es)) ->
    let (t1, ts) = synth env default_typ e, map (synth env default_typ) es in
    let tarr = List.fold_right (typ_union env) ts t1 in
    mk_array_typ p2 env tarr
  | ERef (p, k, e) ->
    let t = synth env default_typ e in
    begin match k with
      | SourceCell -> TSource (None, t)
      | SinkCell -> TSink (None, t)
      | RefCell -> TRef (None, t)
    end
  | EDeref (p, e) -> 
    let typ = ((check_kind p env (expose_simpl_typ env (synth env default_typ e)))) in
    if typ = TPrim "Unsafe" 
    then raise (Typ_error (p, FixedString "synth: Cannot dereference an unsafe value"))
    else begin match extract_ref ("EDeref: " ^ (string_of_exp e)) p env typ with
    | TRef (_, t) -> t
    | TSource (_, t) -> t
    | TRegex _ -> (match expose_simpl_typ env (TId "String") with TRef (_, t) -> t | _ -> failwith "Impossible")
    | t -> raise (Typ_error (p, Typ((fun t -> sprintf "synth: cannot read an expression of type %s" (string_of_typ t)), t)))
    end 
  | ESetRef (p, e1, e2) -> 
    let t = extract_ref "ESetRef" p env (expose_simpl_typ env (synth env default_typ e1)) in
    begin match t with
    | TRef (_, TUninit ty) -> begin match !ty with
      | None -> 
        let newTy = synth env default_typ e2 in
        (* Printf.printf "Updating typ at %s to %s\n" (string_of_position p) (string_of_typ newTy); *)
        ty := Some newTy;
        newTy
      | Some s -> (* Printf.printf "Checking typ at %s\n" (string_of_position p); *) check env default_typ e2 s; 
        s
    end
    | TRef (_, TPrim "Unsafe") -> TPrim "Unsafe" (* BSL: PUNTING ON ASSIGNMENT TO UNSAFE *)
    | TRef (_, s) 
    | TSink (_, s) -> 
      (* Printf.eprintf "Synth ESetRef: Checking that %s has type %s\n" (string_of_exp e2) (string_of_typ s); *)
      check env default_typ e2 s; s
    | s -> 
      typ_mismatch p (Typ((fun t -> sprintf "left-hand side of assignment has type %s" (string_of_typ t)), s));
      s
    end
  | ELabel (p, lbl, e) -> 
    (* This is a valid assumption for JavaScript. *)
    (* Printf.eprintf "Synth: Binding label %s in expression %s\n" lbl (string_of_exp e); *)
    check (bind_lbl lbl (TPrim "Undef") env) default_typ e (TPrim "Undef");
    TPrim "Undef"
  | EBreak (p, lbl, e) ->
    let lbl_typ = 
      try lookup_lbl lbl env
      with Not_found -> (* severe *)
        raise (Typ_error (p, String (sprintf "synth: label %s is not defined", lbl))) in
    check env default_typ e lbl_typ;
    TBot
  | ETryCatch (_, e1, x, e2) ->
    let t1 = synth env default_typ e1
    and t2 = synth (bind_id x TTop env) default_typ e2 in
    typ_union env t1 t2
  | ETryFinally (_, e1, e2) -> 
    let _ = synth env default_typ e1 in
    synth env default_typ e2
  | EThrow (_, e) -> 
    let _ = synth env default_typ e in
    TBot
  | ETypecast (p, rt, e) -> 
    let t = synth env default_typ e in
    static env rt t
  | EIf (p, e1, e2, e3) ->
    (match synth env default_typ e1 with
    | TPrim "True" -> synth env default_typ e2
    | TPrim "False" -> synth env default_typ e3
    | t (* when subtype env t typ_bool *) ->
      typ_union env (synth env default_typ e2) (synth env default_typ e3)
    (* | _ -> raise (Typ_error (p, FixedString "synth: Got non-bool type for condition of if expression")) *) )
  | EObject (p, fields) ->
    let mk_field (name, exp) = 
      (P.singleton name, Present, synth env default_typ exp) in
    let get_names (name, _) names = 
      P.union names (P.singleton name) in
    let rest = List.fold_right get_names fields (P.singleton "__proto__") in
    if List.mem "__proto__" (map fst fields) then
      TObject (mk_obj_typ (map mk_field fields) (P.negate rest))
    else
      TObject (mk_obj_typ ((P.singleton "__proto__", Present, TId "Object") 
                           :: (map mk_field fields))
                          (P.negate rest))
  | EBracket (p, obj, field) -> 
    begin match expose_simpl_typ env (synth env default_typ field) with
      | TRegex pat -> begin
        match synth env default_typ obj with
        | TRegex _ -> inherits p env (TId "String") pat
        | obj_typ -> inherits p env (un_null obj_typ) pat
      end
      | idx_typ -> 
        raise (Typ_error
                 (p, Typ((fun t -> sprintf "index has type %s" (string_of_typ t)), idx_typ)))
    end
  | EUpdate (p, obj, field, value) -> begin
    let tobj = synth env default_typ obj in
    (* Printf.eprintf "EUpdate1: Synthed type for object %s is\n%s\n" (string_of_exp obj) (string_of_typ tobj); *)
    (* Printf.eprintf "EUpdate2: Synthed type for value %s is\n%s\n" (string_of_exp value) (string_of_typ (synth env default_typ value)); *)
    (* Printf.eprintf "EUpdate3: %s\n" (string_of_bool (subtype env (synth env default_typ value) (expose env (TId "Ext")))); *)
    match expose_simpl_typ env tobj, 
      expose_simpl_typ env (synth env default_typ field)(* , synth env default_typ value *) with
        | TObject o, TRegex idx_pat (* (TRegex idx_pat as tfld), typ *) ->
          begin
            if not (P.is_subset (pat_env (tid_env env)) 
                                idx_pat (cover_pat o))
              then
                typ_mismatch p (Pat((fun p -> sprintf "synth: %s affects hidden fields" (P.pretty p)), idx_pat))
          end;
          begin
            if P.is_overlapped idx_pat (absent_pat o) then
              (* Printf.eprintf "ERROR: %s failed at field '%s' \nwith type %s, \nwhen absent_pat = %s in typ %s\n" (string_of_exp exp) (P.pretty idx_pat) (string_of_typ typ) (P.pretty (absent_pat o)) (string_of_typ tobj); *)
              typ_mismatch p (PatPatTyp((fun p1 p2 t -> sprintf "synth: Assigning to field '%s' when absent_pat = %s in typ %s" (P.pretty p1) (P.pretty p2) (string_of_typ t)), idx_pat, (absent_pat o), tobj))
          end;
          let fs : field list = fields o in
          let okfield (fld_pat, pres, s) = 
            if P.is_overlapped fld_pat idx_pat (* && not (subtype env (expose env typ) (expose env s)) *) then
              (* try *)
              (*   with_typ_exns check  *)
              (* typ_mismatch p *)
              (*   (TypTypTyp((fun t1 t2 t3 ->  *)
              (*     sprintf "synth, field update: %s, the new value, is not subtype of %s at index %s" *)
              (*       (string_of_typ t1) (string_of_typ t2) (string_of_typ t3)), typ, s, tfld)) in *)
              check env default_typ value s in
          let _ = List.iter okfield fs in
          tobj
        | obj, fld ->
          let _ = typ_mismatch p (TypTyp((fun t1 t2 -> 
            sprintf "Bad update: expected TObject, TRegex, and got %s and %s"
              (string_of_typ t1) (string_of_typ t2)), obj, fld)) in
          obj
          
  end
  | EPrefixOp (p, op, e) -> synth env default_typ (EApp (p, EId (p, op), [e]))
  | EInfixOp (p, op, e1, e2) -> synth env default_typ (EApp (p, EId (p, op), [e1; e2]))
  | EApp (p, f, args) -> 
    let rec check_app tfun =
      (* Printf.eprintf "Checking EApp@%s with function type %s\n" (string_of_position p) (string_of_typ tfun); *)
      begin match expose_simpl_typ env tfun with 
        | TArrow (expected_typs, None, result_typ) -> 
          let args = fill (List.length expected_typs - List.length args) 
            (EConst (p, JavaScript_syntax.CUndefined)) args in
          begin
            try List.iter2 (check env default_typ) args expected_typs
            with Invalid_argument "List.iter2" -> 
              typ_mismatch p
                (NumNum(sprintf "arity-mismatch:  %d args expected, but %d given",
                        (List.length expected_typs), (List.length args)))
          end;
          result_typ
        | TArrow (expected_typs, Some vararg_typ, result_typ) -> 
          if (List.length expected_typs > List.length args) then
            let args = fill (List.length expected_typs - List.length args) 
              (EConst (p, JavaScript_syntax.CUndefined)) args in
            List.iter2 (check env default_typ) args expected_typs
          else begin
            let (req_args, var_args) = ListExt.split_at (List.length expected_typs) args in
            let req_args = fill (List.length expected_typs - List.length req_args)
              (EConst (p, JavaScript_syntax.CUndefined)) req_args in
            List.iter2 (check env default_typ) req_args expected_typs;
            List.iter (fun t -> check env default_typ t vararg_typ) var_args
          end;
          result_typ
        | TIntersect (n, t1, t2) -> 
          with_typ_exns
            (fun () ->
              try 
                let r1 = check_app t1 in 
                begin 
                  try
                    let r2 = check_app t2 in
                    apply_name n (typ_intersect env r1 r2)
                  with | Typ_error _ -> r1
                end
              with | Typ_error _ -> check_app t2)
        | TUnion (n, t1, t2) ->
          let typ_or_err t = with_typ_exns (fun () -> try Some (check_app t) with Typ_error _ -> None) in
          let r1 = typ_or_err t1 in
          let r2 = typ_or_err t2 in
          (match r1, r2 with
          | Some r, None
          | None, Some r -> apply_name n r
          | _ -> raise (Typ_error (p, FixedString "synth: Ambiguous union of functions")))
        | (TForall _) as quant_typ -> 
          begin match Typ.forall_arrow quant_typ with
            | None -> 
              raise (Typ_error (p, Typ((fun t -> sprintf "synth: expected function, got %s"
                (string_of_typ t)), quant_typ)))
            | Some (typ_vars, (TArrow (expected_typs, _, r) as arrow_typ)) -> 
              (* guess-work breaks bidirectionality *)
              let arg_typs = map (synth env default_typ) args in
              let assumed_arg_exps = 
                List.map2 (fun e t -> ECheat (p, t, e)) args arg_typs in
              let assoc = typ_assoc env arrow_typ (TArrow (arg_typs, None, r)) in
              let guess_typ_app exp typ_var = 
                try
                  let guessed_typ = 
                    try IdMap.find typ_var assoc
                    with Not_found ->
                      if (List.length expected_typs > List.length args) 
                      then (TPrim "Undef")
                      else raise Not_found in
                  ETypApp (p, exp, guessed_typ) 
                with Not_found -> begin
                  raise (Typ_error (p, FixedString (sprintf "synth: could not instantiate typ_var %s" typ_var))) end in
              let guessed_exp = 
                fold_left guess_typ_app (ECheat (p, quant_typ, f)) 
                  typ_vars in
              synth env default_typ (EApp (p, guessed_exp, assumed_arg_exps))
            | Some _ -> failwith "expected TArrow from forall_arrow"
          end
        | not_func_typ -> 
          (* even in an intersection, this should count as a genuine error *)
          raise (Typ_error (p,
                            Typ((fun t -> sprintf "expected function, got %s" (string_of_typ t)), not_func_typ)))
      end in 
    (* Printf.eprintf "Synth EApp: Checking function body\n"; *)
    check_app (un_null (synth env default_typ f))
  | ERec (p, binds, body) -> 
    (* No kinding check here, but it simply copies the type from the function.
       Let it work. (Actual reason: no position available) *)
    let f env (x, t, e) = bind_id x t env in
    let env = fold_left f env binds in
    let tc_bind (x, t, e) = check env default_typ e t in
    List.iter tc_bind binds;
    synth env default_typ body 
  | EFunc (p, args, func_info, body) -> 
    (* BSL: Synthesizing Ext *)
    let thisType = if usesThis body then TThis (TId "Ext") else TThis (TPrim "Unsafe") in
    let arrowTyp = TArrow([thisType], Some (TId "Ext"), TId "Ext") in
    (* Printf.eprintf "Checking expression for Ext-arrow:\n%s\n" (string_of_exp exp); *)
    check env default_typ exp arrowTyp;
    arrowTyp
  | ESubsumption (p, t, e) ->
    let t = check_kind p env t in
    check env default_typ e t;
    t
  | EAssertTyp (p, t, e) ->
    (* Printf.eprintf "Synth: AssertTyp that %s has type %s\n" (string_of_exp e) (string_of_typ t); *)
    (* Printf.eprintf "%s\n" (string_of_bool (subtype env (synth env default_typ e) t)); *)
    let t = check_kind p env t in
    let _ = check env default_typ e t in
    t
  | EDowncast (p, t, e) -> 
    let t = check_kind p env t in
    ignore (synth env default_typ e);
    t
  | ETypAbs (p, x, t, e) ->
    (* bind_typ_id does the kinding check *)
    let env = bind_typ_id x t env in
    TForall (None, x, t, synth env default_typ e)
  | ETypApp (p, e, u) ->
    begin match expose_simpl_typ env (synth env default_typ e) with
      | TForall (n, x, s, t) ->
        if subtype env u s then
          apply_name n (typ_subst x u t)
        else 
          begin
            typ_mismatch p
              (TypTyp((fun t1 t2 -> sprintf "type-argument %s is not a subtype of the bound %s"
                 (string_of_typ t1) (string_of_typ t2)), u, s));
            typ_subst x s t (* Warning: produces possibily spurious errors *)
          end
      | t ->
        raise
          (Typ_error (p, TypTyp((fun t1 t2 -> 
            sprintf "expected forall-type in type application, got:\n%s\ntype argument is:\n%s"
              (string_of_typ t1) (string_of_typ t2)), t, u)))
    end
  | ECheat (p, t, _) -> expose_simpl_typ env t
  | EParen (p, e) -> synth env default_typ e

and synths env default_typ es = map (synth env default_typ) es

let typecheck env default_typ exp =
  let _ = synth env default_typ exp in
  ()

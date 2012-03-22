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
  | TUnion (TPrim "Undef", t') -> t'
  | TUnion (t', TPrim "Undef") -> t'
  | TUnion (TPrim "Null", t') -> t'
  | TUnion (t', TPrim "Null") -> t'
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

let extract_ref msg p env t = 
  let rec helper t = match expose_simpl_typ env t with
    | TPrim _  -> None
    | TRef _ as t -> Some t
    | TSource _ as t -> Some t
    | TSink _ as t -> Some t
    | TRec _ as t -> helper (simpl_typ env t)
    | TUnion (t1, t2) -> 
      let r1 = helper t1 in
      let r2 = helper t2 in
      (match r1, r2 with
      | Some r, None
      | None, Some r -> Some r
      | _ -> None) 
    | TIntersect (t1, t2) -> 
      let r1 = helper t1 in
      let r2 = helper t2 in
      (match r1, r2 with
      | Some r, None
      | None, Some r -> Some r
      | _ -> None)
    | TForall _ -> Some t (* BSL : This seems incomplete; extract_ref won't descend under a Forall *)
    | _ -> (* (printf "%s: Got to %s\n" msg (string_of_typ t)); *) None in
  match helper t with
  | Some t -> t
  | None -> raise (Typ_error (p, StringTyp((fun s t -> sprintf "%s: Ambiguous ref type for type %s"
    s (string_of_typ t)), msg, t)))

let extract_arrow env t = 
  let rec helper t = match expose_simpl_typ env t with
    | TArrow _ as t -> Some t
    | TUnion (t1, t2) -> 
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


let rec check (env : env) (default_typ : typ option) (exp : exp) (typ : typ) : unit =
  try check' env default_typ exp typ
  (* typ_mismatch normally records the error and proceeds. If we're in a
     [with_typ_exns] context, it will re-raise the error. *)
  with Typ_error (p, s) -> typ_mismatch p s
    
and check' (env : env) (default_typ : typ option) (exp : exp) (typ : typ) : unit = match exp with
  | ELabel (p, lbl, e) ->
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
    | SourceCell, TSource t
    | SinkCell, TSink t
    | RefCell, TRef t -> check env default_typ e t
    | _, t  -> typ_mismatch p (* TODO(arjun): error msg *)
             (TypTyp((fun t1 t2 -> sprintf "!!expected %s, got %s"
               (string_of_typ t1) (string_of_typ t2)), t, (synth env default_typ e)))
    end
  | EArray (p, es) ->
      let expect_elt_typ = 
        simpl_lookup p (tid_env env)
          (un_null (expose_simpl_typ env typ)) array_idx_pat in
      let expect_array_typ = mk_array_typ p env expect_elt_typ in
      (if not (subtype env (TRef typ) expect_array_typ) then
         typ_mismatch p 
           (TypTyp((fun t1 t2 -> sprintf "check: expected Array<%s>, got %s" (string_of_typ t1) (string_of_typ t2)), expect_elt_typ, (TRef typ))));
      List.iter (fun e -> check env default_typ e expect_elt_typ) es 
  | EParen (_, e) -> check env default_typ e typ
  | EObject (p, fields) -> begin match expose_simpl_typ env typ with
    | TObject _ as t -> 
      let absPat = P.negate (List.fold_left P.union P.empty 
                               (List.map (fun (f, _) -> P.singleton f) fields)) in
      let newObjTyp = TObject (mk_obj_typ 
                                 (List.map (fun (fieldName, fieldExp) -> 
                                   let fieldTyp = (inherits p env t (P.singleton fieldName)) in
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
    (* Printf.eprintf "Synthing type for %s\n" (string_of_exp exp); *)
    let synth_typ = expose_simpl_typ env (synth env default_typ exp) in
    (* Printf.printf "Checking %s <?: %s\n" (string_of_typ synth_typ) (string_of_typ (expose_simpl_typ env typ)); *)
    if not (subtype env synth_typ (expose_simpl_typ env typ)) then begin
      (* Printf.printf "failed.\n"; *)
      typ_mismatch (Exp.pos exp)
        (TypTyp((fun t1 t2 -> sprintf "%%expected %s, got %s" (string_of_typ t1) (string_of_typ t2)), (expose_simpl_typ env typ), synth_typ))
    end (* else *)
      (* Printf.printf "Checking finished.\n" *)

and synth (env : env) (default_typ : typ option) (exp : exp) : typ = 
    (* Printf.eprintf "*Synthing type for %s\n" (string_of_exp exp); *)
match exp with
  (* TODO: Pure if-splitting rule; make more practical by integrating with
      flow typing. *)
  | EIf (p, EInfixOp (_, "hasfield",  EDeref (_, EId (_, obj)), (EId (_, fld))),
         true_part, false_part) ->
    begin match expose_simpl_typ env (lookup_id obj env), lookup_id fld env with
      | TRef (TObject ot), TRegex pat -> 
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
      | TRef t -> t
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
      | SourceCell -> TSource t
      | SinkCell -> TSink t
      | RefCell -> TRef t
    end
  | EDeref (p, e) -> 
    let typ = ((check_kind p env (expose_simpl_typ env (synth env default_typ e)))) in
    if typ = TPrim "Unsafe" 
    then raise (Typ_error (p, FixedString "synth: Cannot dereference an unsafe value"))
    else begin match extract_ref ("EDeref: " ^ (string_of_exp e)) p env typ with
    | TRef t -> t
    | TSource t -> t
    | t -> raise (Typ_error (p, Typ((fun t -> sprintf "synth: cannot read an expression of type %s" (string_of_typ t)), t)))
    end 
  | ESetRef (p, e1, e2) -> 
    let t = extract_ref "ESetRef" p env (expose_simpl_typ env (synth env default_typ e1)) in
    begin match  t with
    | TRef (TUninit ty) -> begin match !ty with
      | None -> 
        let newTy = synth env default_typ e2 in
        (* Printf.printf "Updating typ at %s to %s\n" (string_of_position p) (string_of_typ newTy); *)
        ty := Some newTy;
        newTy
      | Some s -> (* Printf.printf "Checking typ at %s\n" (string_of_position p); *) check env default_typ e2 s; 
        s
    end
      | TRef (TPrim "Unsafe") -> t (* BSL: PUNTING ON ASSIGNMENT TO UNSAFE *)
      | TRef s 
      | TSink s -> check env default_typ e2 s; s
      | s -> typ_mismatch p (Typ((fun t -> sprintf "left-hand side of assignment has type %s" (string_of_typ t)), s));
        s
    end
  | ELabel (p, lbl, e) -> 
    (* This is a valid assumption for JavaScript. *)
    synth (bind_lbl lbl (TPrim "Undef") env) default_typ e
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
    | t when subtype env t typ_bool ->
      typ_union env (synth env default_typ e2) (synth env default_typ e3)
    | _ -> raise (Typ_error (p, FixedString "synth: Got non-bool type for condition of if expression")))
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
      | TRegex pat -> inherits p env (un_null (synth env default_typ obj)) pat
      | idx_typ -> 
        raise (Typ_error
                 (p, Typ((fun t -> sprintf "index has type %s" (string_of_typ t)), idx_typ)))
    end
  | EUpdate (p, obj, field, value) -> begin
    let tobj = synth env default_typ obj in
    (* Printf.printf "Synthed type for %s of %s\n" (string_of_exp obj) (string_of_typ tobj); *)
    match expose_simpl_typ env tobj, 
      expose_simpl_typ env (synth env default_typ field), synth env default_typ value with
        | TObject o, (TRegex idx_pat as tfld), typ ->
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
            if P.is_overlapped fld_pat idx_pat && not (subtype env typ s) then
              typ_mismatch p
                (TypTypTyp((fun t1 t2 t3 -> 
                  sprintf "synth, field update: %s, the new value, is not subtype of %s at index %s"
                    (string_of_typ t1) (string_of_typ t2) (string_of_typ t3)), typ, s, tfld)) in
          let _ = List.iter okfield fs in
          tobj
        | obj, fld, typ ->
          let _ = typ_mismatch p (TypTypTyp((fun t1 t2 t3 -> 
            sprintf "Bad update: %s[%s = %s]"
              (string_of_typ t1) (string_of_typ t2) (string_of_typ t3)), obj, fld, typ)) in
          obj
          
  end
  | EPrefixOp (p, op, e) -> synth env default_typ (EApp (p, EId (p, op), [e]))
  | EInfixOp (p, op, e1, e2) -> synth env default_typ (EApp (p, EId (p, op), [e1; e2]))
  | EApp (p, f, args) -> 
    let rec check_app tfun =
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
        | TIntersect (t1, t2) -> 
          with_typ_exns
            (fun () ->
              try 
                let r1 = check_app t1 in 
                begin 
                  try
                    let r2 = check_app t2 in
                    typ_intersect env r1 r2
                  with | Typ_error _ -> r1
                end
              with | Typ_error _ -> check_app t2)
        | TUnion (t1, t2) ->
          let typ_or_err t = with_typ_exns (fun () -> try Some (check_app t) with Typ_error _ -> None) in
          let r1 = typ_or_err t1 in
          let r2 = typ_or_err t2 in
          (match r1, r2 with
          | Some r, None
          | None, Some r -> r
          | _ -> raise (Typ_error (p, FixedString "synth: Ambiguous union of functions")))
        | (TForall _) as quant_typ -> 
          begin match Typ.forall_arrow quant_typ with
            | None -> 
              raise (Typ_error (p, Typ((fun t -> sprintf "synth: expected function, got %s"
                (string_of_typ t)), quant_typ)))
            | Some (typ_vars, (TArrow (_, _, r) as arrow_typ)) -> 
              (* guess-work breaks bidirectionality *)
              let arg_typs = map (synth env default_typ) args in
              let assumed_arg_exps = 
                List.map2 (fun e t -> ECheat (p, t, e)) args arg_typs in
              let assoc =
                typ_assoc env arrow_typ (TArrow (arg_typs, None, r)) in
              let guess_typ_app exp typ_var = 
                try
                  let guessed_typ = IdMap.find typ_var assoc in
                  ETypApp (p, exp, guessed_typ) 
                with Not_found -> begin
                  raise (Typ_error (p, FixedString "synth: $$$ could not instantiate")) end in
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
    check_app (un_null (synth env default_typ f))
  | ERec (binds, body) -> 
    (* No kinding check here, but it simply copies the type from the function.
       Let it work. (Actual reason: no position available) *)
    let f env (x, t, e) = bind_id x t env in
    let env = fold_left f env binds in
    let tc_bind (x, t, e) = check env default_typ e t in
    List.iter tc_bind binds;
    synth env default_typ body 
  | EFunc (p, args, func_info, body) -> 
    (* BSL: Synthesizing Ext *)
    let arrowTyp = TArrow([TId "Ext"], Some (TId "Ext"), TId "Ext") in
    (* Printf.eprintf "Checking expression for Ext-arrow:\n%s\n" (string_of_exp exp); *)
    check env default_typ exp arrowTyp;
    arrowTyp
  | ESubsumption (p, t, e) ->
    let t = check_kind p env t in
    check env default_typ e t;
    t
  | EAssertTyp (p, t, e) ->
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
    TForall (x, t, synth env default_typ e)
  | ETypApp (p, e, u) ->
    begin match expose_simpl_typ env (synth env default_typ e) with
      | TForall (x, s, t) ->
        if subtype env u s then
          typ_subst x u t
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

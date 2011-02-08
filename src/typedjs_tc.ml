open Prelude
open Typedjs_syntax
open Typedjs_env
open Typedjs_types 
open Format
open Typedjs_dyn
open Typedjs_tc_util

let dummy = (Lexing.dummy_pos, Lexing.dummy_pos)

let string_of_typ' = Env.Pretty.string_of_typ'

let contracts : (int * typ) IntMap.t ref = ref IntMap.empty

let error_on_unreachable = ref true

let disable_unreachable_check () =
  error_on_unreachable := false

let print_unreachable_apps = ref false

let is_print_native = ref false

let print_native () = 
  is_print_native := true

let rec skip n l = if n == 0 then l else (skip (n-1) (List.tl l))
let rec fill n a l = if n <= 0 then l else fill (n-1) a (List.append l [a])

let map_to_list m = IdMap.fold (fun k v l -> (k,v)::l) m []

let error p s = raise (Typ_error (p, s))

let idmap_to_list map =
  IdMap.fold (fun x s xs -> (x, s)::xs) map []

let class_fields_list env cnames = 
  List.fold_right (fun cname l ->
                     l@(map_to_list (Env.class_fields env cname)))
    cnames []

let class_types (env : Env.env) constr = 
  IdMapExt.values (Env.class_fields env constr)

let class_names (env : Env.env) constr = 
  IdMapExt.keys (Env.class_fields env constr)

let class_types_list env cnames = 
  List.fold_right (fun cname l -> l@(class_types env cname)) cnames []
  
let string_of_typ_list ts = 
  fold_left (^) "" (intersperse "," (map string_of_typ ts))

let string_of_str_list ts = 
  fold_left (^) "" (intersperse "," ts)

let unfold_typ t = match t with
  | TRec (x, t') -> Typedjs_syntax.Typ.typ_subst x t t'
  | _ -> t

let rec typ_to_list t = match t with
  | TUnion (t1, t2) -> (typ_to_list t1)@(typ_to_list t2)
  | TRec _ -> typ_to_list (unfold_typ t)
  | t -> [t]

let rec list_to_typ env ts = match ts with
  | (t::rest) -> Env.typ_union env t (list_to_typ env rest)
  | [] -> TBot

(* Things that lead to errors when applied *)
let applicable t = match t with
  | TConstr _ -> false (* no constructor is applicable *)
  | t -> true

let applicables env t = 
  let filtered = (List.filter applicable (typ_to_list t)) in
    list_to_typ env filtered

let un_null t = match t with
  | TUnion (TConstr ("Undef", []), t') -> t'
  | TUnion (t', TConstr ("Undef", [])) -> t'
  | TUnion (TConstr ("Null", []), t') -> t'
  | TUnion (t', TConstr ("Null", [])) -> t'
  | _ -> t

let rec un_ref t = match t with
  | TRef s -> s
  | TSource s -> s
  | TSink s -> s
  | TUnion (t1, t2) -> TUnion (un_ref t1, un_ref t2) 
  | TConstr ("Undef", []) -> t
  | _ -> failwith ("un_ref got " ^ string_of_typ t)

let rec tc_exp_simple (env : Env.env) exp = match exp with
  | EConst (_, c) -> tc_const c
  | EBot _ -> TBot
  | EId (p, x) -> begin
      try 
        Env.lookup_id x env
      with Not_found -> raise (Typ_error (p, x ^ " is not defined"))
    end
  | ELet (_, x, e1, e2) -> tc_exp (Env.bind_id x (tc_exp env e1) env) e2
  | ESeq (_, (EApp (p2, ef, [(ETypecast (p5, typ, EThis _))
                              as arg]) 
               as app), e2) -> 
      begin match tc_exp env ef with
        | TIntersect (TArrow (this1, [arg1], rest1, TBot), 
                      TArrow (this2, [arg2], rest2, tc))
            when not (Env.subtype env tc TBot) ->
            let env_cont = Env.bind_id "this" arg2 env in
              tc_exp env_cont e2
        | _ ->
            ignore (tc_exp env app);
            tc_exp env e2
      end
  | ESeq (_, e1, e2) -> begin match tc_exp env e1 with
        TBot -> (* e1 will not return; no need to typecheck e2 *)
          TBot
      | _ -> tc_exp env e2
    end
  | ERef (p, k, e) ->
    let t = tc_exp env e in
    begin match k with
      | SourceCell -> TSource t
      | SinkCell -> TSink t
      | RefCell -> TRef t
    end
  | EDeref (p, e) -> begin match tc_exp env e with
      | TRef t -> t
      | TSource t -> t
      | TField -> TField
      | TUnion (t1, t2) -> 
          Env.typ_union env (unfold_typ (un_ref t1)) (unfold_typ (un_ref t2))
      | t -> raise (Typ_error (p, "cannot read an expression of type " ^
				 (string_of_typ t))) 
    end
  | ESetRef (p, e1, e2) -> begin match un_null (tc_exp env e1), 
                                       tc_exp env e2 with
      | TRef s, t
      | TSink s, t ->
          if Env.subtype env t s then 
            t
          else raise
            (Typ_error 
               (p, sprintf "left-hand side has type \n\n %s, \n\n but the \
                  right-hand side has type \n\n %s"
                  (string_of_typ s) (string_of_typ t)))
      | s, t -> 
          raise (Typ_error 
                   (p, sprintf "cannot write to LHS (%s \n := \n %s)" 
                      (string_of_typ s) (string_of_typ t)))
    end
  | ELabel (p, l, t, e) -> 
      let t = Env.check_typ p env t in
      let s = tc_exp (Env.bind_lbl l (Env.check_typ p env t) env) e in
        if Env.subtype env s t then t
        else raise (Typ_error (p, sprintf "label type mismatch, expected %s, \
                         got %s at %s" (string_of_typ t) (string_of_typ s) l))
  | EBreak (p, l, e) ->
      let s = 
        try Env.lookup_lbl l env
        with Not_found -> 
          raise (Typ_error (p, "label " ^ l ^ " is not defined"))
      and t = tc_exp env e in
        if Env.subtype env t s then TBot
        else raise
          (Typ_error 
             (p,
              match l with
                  "%return" -> sprintf 
                    "this expression has type %s, but the function\'s return \
                     type is %s" (string_of_typ t) (string_of_typ s)
                | _ -> (* This should not happen. Breaks to labels always have 
                          type typ_undef *)
                    sprintf "this expression has type %s, but the label %s has \
                          type %s" (string_of_typ t) l (string_of_typ s)))
  | ETryCatch (_, e1, x, e2) ->
      let t1 = tc_exp env e1
      and t2 = tc_exp (Env.bind_id x TTop env) e2 in
        Env.typ_union env t1 t2
  | ETryFinally (_, e1, e2) -> 
      let _ = tc_exp env e1 in
        tc_exp env e2
  | EThrow (_, e) -> 
      let _ = tc_exp env e in
        TBot
  | ETypecast (p, rt, e) -> 
      let t = tc_exp env e in
        Env.static env rt t
  | EEmptyArray (p, elt_typ) -> 
      TConstr ("Array", [ Env.check_typ p env elt_typ ])
  | EArray (p, []) -> 
      raise (Typ_error (p, "an empty array literal requires a type annotation"))
  | EArray (p, e :: es) -> 
      (* We are promoting the type of values in each element to the union *)
      let f t1 t2 = match t1, t2 with
        | TRef s1, TRef s2 -> TRef (Env.typ_union env s1 s2)
        | _ -> failwith "expected Ref cells in array" in
      let u = fold_left f (tc_exp env e) (map (tc_exp env) es) in
        (* hack to make arrays not have undefined elements: *)
        Env.check_typ p env (TConstr ("Array", [u]))
  | EIf (p, (EApp (p2, ef, [(ETypecast (p5, typ, EId (p3, x)))
                              as arg]) 
               as app), 
         e2, e3) ->
      let f_typ = tc_exp env ef in
      let a_typ = tc_exp env arg in
        (match f_typ with
           | TIntersect (TArrow (this1, [arg1], rest1, TConstr ("True", [])), 
                         TArrow (this2, [arg2], rest2, tc)) 
               when Env.subtype env tc typ_bool ->
               if Env.subtype env arg1 a_typ && Env.subtype env arg2 a_typ then
                 let env_true = Env.bind_id x arg1 env in
                 let env_false = Env.bind_id x arg2 env in
                   Env.typ_union env (tc_exp env_true e2) (tc_exp env_false e3)
               else 
                 (ignore (tc_exp env app);
                  Env.typ_union env (tc_exp env e2) (tc_exp env e3))
           | _ ->
               ignore (tc_exp env app);
               Env.typ_union env (tc_exp env e2) (tc_exp env e3))
  | EIf (p, (EPrefixOp (p6, "prefix:!",
                        (EApp (p2, ef, [(ETypecast (p5, typ, EId (p3, x)))
                                          as arg]) 
                           as app))), 
         e2, e3) ->
      let f_typ = tc_exp env ef in
      let a_typ = tc_exp env arg in
        (match f_typ with
           | TIntersect (TArrow (this1, [arg1], rest1, TConstr ("True", [])), 
                         TArrow (this2, [arg2], rest2, tc)) 
               when Env.subtype env tc typ_bool ->
               if Env.subtype env arg1 a_typ && Env.subtype env arg2 a_typ then
                 let env_true = Env.bind_id x arg2 env in
                 let env_false = Env.bind_id x arg1 env in
                   Env.typ_union env (tc_exp env_true e2) (tc_exp env_false e3)
               else 
                 (printf "Warning, in else case of if-split rule";
                  ignore (tc_exp env app);
                  Env.typ_union env (tc_exp env e2) (tc_exp env e3))
           | _ ->
               ignore (tc_exp env app);
               Env.typ_union env (tc_exp env e2) (tc_exp env e3))
  | EIf (p, e1, e2, e3) ->
      ignore (tc_exp env e1);
      Env.typ_union env (tc_exp env e2) (tc_exp env e3)
  | EObject (p, fields) ->
      Env.check_typ p env 
      (TObject (map (second2 (tc_exp env)) fields))
  | EBracket (p, obj, field) -> let typ = un_null (tc_exp env obj) in
      let t_list = typ_to_list typ in
      let ft = tc_exp env field in
      let f_list = map (bracket p env ft) t_list in
        list_to_typ env f_list
  | EUpdate (p, obj, field, newval) ->
      let typ = un_null (tc_exp env obj) in
      let t_list = typ_to_list typ in
      let f_list = map (update p env field newval) t_list in
        list_to_typ env f_list
  | EThis p -> begin
      try
        Env.lookup_id "this" env
      with Not_found -> raise (Typ_error (p, "'this' used in non-func"))
    end
  | ENew (p, cid, args) ->
      (* treat it as a function application, but return the class
         type instead of the return type *)
      (if not (Env.is_class env cid) then error p ("undefined class: " ^ cid));
      let _ = tc_exp env (EApp (p, EDeref (p, EId (p, cid)), args)) in
        TConstr (cid, [])
  | EPrefixOp (p, op, e) -> tc_exp env (EApp (p, EId (p, op), [e]))
  | EInfixOp (p, "instanceof", e1,
              EConst (_, JavaScript_syntax.CString constr_name)) ->
    let _ = tc_exp env e1 in
    if Env.is_class env constr_name then
      typ_bool
    else
      error p (sprintf "%s is not a known constructor" constr_name)
  | EInfixOp (p, "+", e1, e2) -> 
      let t1 = tc_exp env e1 in
      let t2 = tc_exp env e2 in
        if (Env.subtype env t1 typ_str || Env.subtype env t2 typ_str) then
          typ_str
        else 
          tc_exp env (EApp (p, EId (p, "+"), [e1; e2]))
  | EInfixOp (p, op, e1, e2) -> tc_exp env (EApp (p, EId (p, op), [e1; e2]))
  | EApp (p, EDeref (_, EBracket (_, obj, EConst (_, JavaScript_syntax.CString "apply"))),
          [e_this; e_args]) -> 
      let tfun =  applicables env (tc_exp env obj) in
        begin match tfun with
          | TObjStar (_, _, _,
                      TArrow (expected_thist, expected_typs, rest_typ, result_typ))
          | TArrow (expected_thist, expected_typs, rest_typ, result_typ) ->
              let this_t = tc_exp env e_this in
              let allow = 
                (match expected_thist, tc_exp env e_args with
                   | ThisIs expected, TList (args) -> args = expected_typs && 
                    Env.subtype env this_t expected
                   | _ -> error p "Need a list type for apply") in
                if allow then result_typ else 
                  error p "This type didn't match in apply"
          | _ -> error p (sprintf "Not an arrow type in apply: %s"
                            (string_of_typ tfun)) 
      end
  | EApp (p, f, args) -> 
      let fn_typ = tc_exp env f in
      if !is_print_native then
        printf "%s: %s\n" (string_of_position p)
          (if Env.subtype env (TConstr ("Native", [])) fn_typ then
             "native function called"
           else
             "safe application");
      begin match applicables env fn_typ with
      | TForall (x, _, TArrow (obj_typ, expected_typ, rest_typ, result_typ)) ->
          let subst = unify_typ
            (TArrow (obj_typ, expected_typ, rest_typ, result_typ))
            (TArrow (obj_typ, map (tc_exp env) args, rest_typ, result_typ)) in
            begin try
              let u = IdMap.find x subst in (* TODO: needless recomputation *)
                tc_exp env (EApp (p, ETypApp (p, f, u), args))
            with Not_found ->
              let t = TArrow (obj_typ, expected_typ, rest_typ, result_typ) in
              error p (sprintf "could not determine \'%s in the function type \
                                %s" x (string_of_typ t))
            end
      | TObjStar (_, _, _, 
                  TArrow (expected_thist, expected_typs, rest_typ, result_typ))
      | TArrow (expected_thist, expected_typs, rest_typ, result_typ) ->
          let _ = (
            let this_typ = tc_thist env f in
              match expected_thist with
                | NoThis -> this_typ
                | ThisIs t -> 
                    if not (Env.subtype env this_typ t) then
                      raise (Typ_error 
                               (p, sprintf "expected this type %s, \n\n got %s"
                                  (string_of_typ t)
                                  (string_of_typ this_typ)))
                    else this_typ) in
          let arg_typs' = map (tc_exp_ret env) args in
          let arg_typs = 
            fill (List.length expected_typs - List.length args) 
              typ_undef arg_typs' in
          let expected_typs =
            fill (List.length args - List.length expected_typs)
              rest_typ expected_typs in
            if Env.subtypes env arg_typs expected_typs 
            then result_typ 
            else if List.length args = List.length expected_typs (* || 
                 dont need to supply undefined arguments =) 
              (List.length args < List.length expected_typs && 
                 (List.iter (
                    fun t -> if Env.subtype env typ_undef t then () else
                      raise (
                        Typ_error (
                          p, "argument that can't be undefined not given")))
                    (skip (List.length args) expected_typs);true)) *)
            then
              let typ_pairs = List.combine arg_typs expected_typs in
              let arg_ix = ref 1 in
              let find_typ_err (arg_typ, expected_typ) = 
                if Env.subtype env arg_typ expected_typ 
                then (incr arg_ix; false)
                else true in
              let arg_typ, expected_typ = List.find find_typ_err typ_pairs in
                raise (Typ_error 
                         (p, sprintf "argument %d has type %s, but the \
                               function expects an argument of type %s,
                               \n\n %s" 
                            !arg_ix (string_of_typ arg_typ)
                               (string_of_typ expected_typ)
                               (string_of_typ (tc_exp env (EThis p)))))
            else raise (Typ_error 
                          (p, sprintf "arity-mismatch: the function expects %d \
                                arguments, but %d arguments given."
                             (List.length expected_typs) (List.length args)))
      | (TObjStar _) as o ->
          raise 
            (Typ_error 
               (p, "expected a function, but expression has obj* " ^
                  (string_of_typ o)))
      | t -> 
          raise 
            (Typ_error 
               (p, "expected a function, but expression has type " ^
                  (string_of_typ t)))
    end
  | ERec (binds, body) -> 
      let f env (x, t, e) =
        Env.bind_id x (Env.check_typ (Exp.pos e) env t) env in
      let env = fold_left f env binds in
      let tc_bind (x, t, e)=
        let s = tc_exp env e in
        let t = Env.check_typ (Exp.pos e) env t in
          if Env.subtype env s t then ()
          else (* this should not happen; rec-annotation is a copy of the
                  function's type annotation. *)
            failwith (sprintf "%s is declared to have type %s, but the bound \
                             expression has type %s" x (string_of_typ t)
                        (string_of_typ s)) in
        List.iter tc_bind binds;
        tc_exp env body
  | EFunc (p, args, fn_typ, body) -> 
      let expected_typ = Env.check_typ p env fn_typ in
      let check_arrow env arrow =
        match arrow with
          | TArrow (this_t, arg_typs, rest_typ, result_typ) ->
              if not (List.length arg_typs = List.length args) then
                error p 
                  (sprintf "given %d argument names, but %d argument types"
                     (List.length args) (List.length arg_typs));
              let bind_arg env x t = 
                Env.bind_id x t env in
              let env = List.fold_left2 bind_arg env args arg_typs in
              let env = Env.clear_labels env in
              let env = match this_t with
                | NoThis -> env
                | ThisIs t -> Env.bind_id "this" t env in
              let env = Env.bind_id "arguments" (TList arg_typs) env in
              let body_typ = match tc_def None env body with
                | Some t, _ -> t 
                | None, _ -> raise (Typ_error (p, "Function's body was bad")) in
                if Env.subtype env body_typ result_typ then 
                  expected_typ
                else raise 
                  (Typ_error
                     (p,
                      sprintf "function body has type\n%s\n, but the \
                             return type is\n%s" (string_of_typ body_typ)
                        (string_of_typ result_typ)))
          | _ -> raise (Typ_error (p, "invalid type annotation on a function")) in
        begin match Env.bind_typ env expected_typ with
          | (env, TIntersect (arrow1, arrow2)) ->
              let left = check_arrow env arrow1 in
              let right = check_arrow env arrow2 in
                TIntersect (left, right)
          | (env, arrow) ->
              check_arrow env arrow
      end
  | ESubsumption (p, t, e) ->
      let s = tc_exp env e in
      let t = Env.check_typ p env t in
      if Env.subtype env s t then
        t
      else 
        error p (sprintf "%s \n\nis not a subtype of \n\n%s" 
                   (string_of_typ s) (string_of_typ t))
  | EAssertTyp (p, raw_t, e) ->
      let s = tc_exp env e in
      let t = Env.check_typ p env raw_t in
        if Env.subtype env s t then
          s (* we do not subsume *)
        else
          error p 
            (sprintf "expression has type %s, which is incompatible with the \
                      annotation" (string_of_typ s))
  | EObjCast (p, t, e) ->
      let proto = begin match e with
        | ENew (p, cid, args) -> begin match Env.class_sup env cid with
        | Some c -> TConstr (c, [])
            | None -> TConstr ("Object", [])
          end
        | EObject _ -> TConstr ("Object", [])
        | ERec ([_, _, 
                EFunc (_, _, _, _)], 
                _) 
        | EFunc (_, _, _, _) -> TConstr ("Function", [])
        | EEmptyArray (p, elt_typ) -> TConstr ("Array", [])
        | EArray (p, elts) -> TConstr ("Array", [])
        | EConst (p, JavaScript_syntax.CRegexp _) -> TConstr ("RegExp", [])
        | e -> 
            (match un_null (tc_exp env e) with
               | TFresh (TConstr (cname, args)) -> TConstr (cname, args)
               | _ -> error p (sprintf "Not an object literal or new for ObjCast: %s" 
                              (Typedjs_syntax.string_of_exp e)))
      end in
      let s = tc_exp env e in
      let t = unfold_typ (Env.check_typ p env t) in
      let safe_unref rf = (match rf with
        | TRef t -> t
        | TSource t -> t
        | T_ -> error p "Trying to unref an absent field"
        | t -> error p (sprintf "Trying to unref a non-ref type: %s " (string_of_typ t))) in
        begin match t with
          | TObjStar (fs, proto', other_typ, code) -> 
              if not (Env.subtype env proto proto') then
                error p (sprintf "Mismatched prototypes in ObjCast: \ 
                                 %s" (string_of_typ proto))
              else
                let find_bad_field fss (k, t) =
                  let t = Env.normalize_typ env t in
                  if List.mem_assoc k fss then
                    (if Env.subtype env (safe_unref t) (safe_unref (List.assoc k fss))
                     then None else Some (k, safe_unref t, safe_unref (List.assoc k fss)))
                  else
                    (if Env.subtype env (safe_unref t) (safe_unref other_typ)
                     then None else Some (k, safe_unref t, safe_unref other_typ))
                in
                let ok_field fss (k, t) =
                  if List.mem_assoc k fss then
                    Env.subtype env (safe_unref t) (safe_unref (List.assoc k fss))
                  else Env.subtype env (safe_unref t) (safe_unref other_typ)
                in
                  begin match s with 
                    | TObject (fs') -> 
                        if List.for_all (ok_field fs) fs' then t else
                          error p (sprintf "Invalid ObjCast---bad named \
                                     fields: %s %s" (string_of_typ t) 
                                     (string_of_typ_list (map snd fs')))
                    | TFresh (TConstr (cname, []))
                    | TConstr (cname, []) ->
                        let fs' = map_to_list (Env.class_fields env cname) in
                        let bad_field = List.fold_right
                          (fun (k, t) bad ->
                             match (find_bad_field fs (k, t), bad) with
                               | None, a -> a
                               | Some t1, Some t2 -> bad
                               | Some t1, None -> Some t1) fs' None 
                        in
                          (match bad_field with
                             | None -> t
                             | Some (k, t, t2) ->
                                 error p (sprintf "Invalid ObjCast: %s \n\n %s \n\n </: \n\n %s"
                                          k (string_of_typ t) (string_of_typ t2)))
                    | TConstr ("Array", [tarr]) ->
                        if Env.subtype env tarr other_typ then t else
                          error p (sprintf "Invalid ObjCast---%s </: %s" 
                                    (string_of_typ other_typ) 
                                    (string_of_typ tarr))
                    | TArrow (ths, args, rest, ret) ->
                        if Env.subtype env s code then t else 
                          error p (sprintf "Invalid ObjCast---%s </: %s" 
                                    (string_of_typ s) (string_of_typ code))
                    | t ->
                        error p (sprintf "This shouldn't happen, %s wasn't an \
                                Object type in ObjCast" (string_of_typ t))
                end
          | t -> error p "Must be an objstar to ObjCast"
        end
  | EDowncast (p, t, e) -> 
      let t = Env.check_typ p env t in
      let (p1, p2) = Exp.pos e in 
        contracts := IntMap.add p1.Lexing.pos_cnum (p2.Lexing.pos_cnum, t)
          !contracts;
        ignore (tc_exp env e);
        t
  | ETypAbs (p, x, t, e) ->
      let t = Env.check_typ p env t in
      let env = Env.bind_typ_id x t env in
      TForall (x, t, tc_exp env e)
  | ETypApp (p, e, u) ->
      let u = Env.check_typ p env u in
        begin match tc_exp env e with
          | TForall (x, s, t) ->
              if Env.subtype env u s then
                Typedjs_syntax.Typ.typ_subst x u t
              else 
                error p (sprintf "expected an argument of type %s, got %s"
                           (string_of_typ s) (string_of_typ u))
          | t ->
              error p (sprintf "expected a quantified type (got %s)"
                         (string_of_typ t))
        end
  | EForInIdx _ -> TRef (typ_str)
  | ECheat (p, t, _) -> Env.check_typ p env t
      
and tc_exps env es = map (tc_exp env) es

(* find the first bracketref, return type of lhs, or none otherwise 
obj.foo() --> (deref (deref obj)["foo"]), so we look for this pattern *)
and tc_thist env e = match e with 
  | EDeref (_, EBracket (_, obj, prop)) -> un_null (tc_exp env obj)
  (* let falls through, same w/ seq, labels, etc *)
  | ELet (_, x, e1, e2) -> tc_thist (Env.bind_id x (tc_exp env e1) env) e2
  | ESeq (_, e1, e2) -> begin match tc_exp env e1 with
        TBot -> TObject []
      | _ -> tc_thist env e2
    end
  | ELabel (_, _, _, e1) -> tc_thist env e1
  | ETryCatch (_, exp, _, _) -> tc_thist env exp
  | ETryFinally (_, e1, e2) -> tc_thist env e1
  (*what would etypecast, subsumption, typecast do? probably be none....*)
  | _ -> Env.lookup_global env

(* type-check [e] and ensure that the resulting type is not [TBot]. If 
   [e : TBot], then [e] provably does not return. There are two possibilities:
   
   1. [e] is a control operator. If so, call [tc_exp'] to avoid this check.
   2. [e] is unreachable code. This is not a type-error, but probably 
      unintended.
*)
   
and tc_exp_ret env e = 
  let t = tc_exp env e in
    if !error_on_unreachable && t = TBot then
      error (Exp.pos e) "unreachable code"
    else 
      t

and update p env field newval t =
  let dict_error ot ft vt =
    (Typ_error (p, (sprintf "Dictionary assignment error: \n \
                             Object: %s \n \
                             Field: %s
                             Value: %s, \n"
                             (string_of_typ ot)
                             (string_of_typ ft)
                             (string_of_typ vt)))) in
  let safe_unref t = match t with
    | TSource _ -> 
        raise (Typ_error (p, (sprintf "Can't assign to const field")))
    | T_ -> 
        raise (Typ_error (p, (sprintf "Can't assign to absent field")))
    | TRef t -> t
    | t -> 
        raise (Typ_error (p, (sprintf "Expected a ref type for update, \ 
                                         got %s" (string_of_typ t)))) 
  in
  match t, field with
    | TObject fs, EConst (_, JavaScript_syntax.CString x) ->
        (try
           let ft = safe_unref (snd2 (List.find (fun (x', _) -> x = x') fs)) in
           let vt = tc_exp env newval in
             if Env.subtype env vt ft then vt
             else raise (Typ_error (p, (sprintf "%s is not a subtype of %s in \
                                        %s[%s = %s]" (string_of_typ vt)
                                          (string_of_typ ft) (string_of_typ t)
                                        x (string_of_typ ft))))
         with Not_found ->
           raise (Typ_error (p, "the field " ^ x ^ " does not exist")))
    | TObjStar (fs, proto, other_typ, code), e ->
	let t_field = tc_exp env e in
        let vt = tc_exp env newval in
        let other_typ = un_ref other_typ in
	  (match t_field with
             | TUnion (TConstr ("Str", []), TConstr ("Undef", []))
             | TConstr ("Str", []) -> 
                 let fts = map safe_unref (map snd fs) in
                   if List.for_all (fun t -> Env.subtype env vt t) fts &&
                      Env.subtype env vt other_typ then
                     vt
                   else
                     raise (dict_error t t_field vt)
             | TConstr ("Int", []) ->
                 if Env.subtype env vt other_typ
                 then vt else 
                   raise (dict_error t t_field vt)
             | TStrSet strs ->
                 let flds = List.filter (fun fld -> List.mem (fst fld) strs) fs in
                 let fts = map safe_unref (map snd flds) in
                   if List.for_all (fun t -> Env.subtype env vt t) fts
                     && (List.length flds = List.length strs ||
                         Env.subtype env vt other_typ)
                   then vt else
                     raise (dict_error t t_field vt)
             | TStrMinus strs ->
                 let flds = List.filter (fun fld -> not (List.mem (fst fld) strs)) fs in
                 let fts = map safe_unref (map snd flds) in
                   if List.for_all (fun t -> Env.subtype env vt t) fts &&
                     Env.subtype env vt other_typ then
                       vt
                   else
                     raise (dict_error t t_field vt)
	     | ft -> raise (dict_error t ft vt))
    | TConstr ("Array", [tarr]), eidx ->
        let tidx = tc_exp env eidx in
        let vt = tc_exp env newval in
        let tarr = un_ref tarr in
          (match tidx with
             | TConstr ("Int", []) -> 
                 if Env.subtype env vt tarr then vt
                 else
                  raise (Typ_error (p, "Bad array type"))
             | _ -> raise (Typ_error (p, "Array index must be int")))
    | TConstr (cname, _), e ->
        let vt = tc_exp env newval in
        let ft = tc_exp env e in
        let fts = List.filter (fun fld -> 
                                 Env.string_in_typ ft (fst fld)) 
          (idmap_to_list (Env.class_fields env cname)) in
          if List.for_all (fun t -> Env.subtype env vt (safe_unref (snd t))) fts 
          then vt
          else (match cname with
                  | "Undef" | "Null" -> TBot (* Actually errors *)
                  | "Num" | "Bool" | "Int" | "Str" | "RegExp" -> vt
                  | _ -> raise (dict_error t ft vt))
    | t, e -> raise (Typ_error (p, (sprintf "No object update on non-objects \
                                 yet, got %s" (string_of_typ t))))


and bracket p env ft ot =
  match ot, ft with
    | TFresh t, ft -> bracket p env ft t
    | TArrow (this_t, args_t, rest_t, return_t), ft ->
        (match ft with
           | TStrSet (["call"]) -> 
               (match this_t with
                  | ThisIs tt -> TRef (TArrow (ThisIs ot, tt::args_t, 
                                               rest_t, return_t))
                  | NoThis -> failwith "Fatal, looking up call on NoThis")
           | _ -> error p "Bad lookup on an arrow type")
    | TUnion (t1, t2), _ ->
        Env.typ_union env (bracket p env ft t1) (bracket p env ft t2)
    | TStrSet strs, _
    | TStrMinus strs, _ -> bracket p env ft (TConstr ("Str", []))
    | TObject fs, TStrSet [x] ->
        (try
           snd2 (List.find (fun (x', _) -> x = x') fs)
         with Not_found ->
           raise (Typ_error (p, "the field " ^ x ^ " does not exist")))
    | TObjStar (fs, proto, other, code), _ ->
        if not (Env.subtype env ft (Env.typ_union env typ_str
                                      (Env.typ_union env typ_int typ_undef)))
        then
          error p (sprintf "Index was type %s in \
                                    dictionary lookup" (string_of_typ ft))
        else
          let flds = List.filter (fun fld -> 
                                    (Env.string_in_typ ft (fst fld)) && 
                                      (not ((snd fld) = T_))) fs in
          let fld_typs = list_to_typ env (map Env.typ_or_abs (map snd flds)) in
          let fld_typs = Env.check_typ p env fld_typs in
            (** Don't use the star type if it is absent, or if every field
                is accounted for by the original list *)
          let no_other = (other = T_) or 
            ((Env.subtract_strings ft (map fst fs)) = TStrSet []) in
          let proto_names = Env.subtract_strings ft (map fst flds) in
            (match proto_names, no_other with
                 (** All fields were covered by ft *)
               | TStrSet ([]), _ -> fld_typs
               (** All non-absent fields were covered by ft *)
               | _, true ->
                   (Env.typ_union env fld_typs
                      (bracket p env proto_names proto))
               (** There is some non-absent field that wasn't in ft *)
               | _, false ->
                   (Env.typ_union env fld_typs
                      (Env.typ_union env (bracket p env proto_names proto)
                         other)))
    | TConstr ("Array", [tarr]), tidx ->
        let (p1, p2) = p in
          contracts := IntMap.add p1.Lexing.pos_cnum 
            (* TODO: NotUndef is not a type, but just a contract *)
            (p2.Lexing.pos_cnum, TConstr ("NotUndef", []))
            !contracts;
          begin match tidx with
            | TConstr ("Int", []) -> tarr
            | TStrSet [s] -> begin match s with
                | "length" -> 
                    TRef typ_int
                | "push" ->
                    TRef (TArrow (ThisIs (typ_array tarr), [un_ref tarr], 
                                  typ_undef, typ_undef))
                | "join" ->
                    (match un_ref tarr with
                       | TConstr ("Str", []) ->
                           TRef (TArrow (ThisIs (typ_array tarr),
                                         [typ_str], typ_undef, typ_str))
                       | _ -> error p ("expected array of strings"))
                | "slice" ->
                    TRef (TArrow (ThisIs (typ_array tarr),
                                  [typ_int; 
                                   Env.typ_union env typ_int typ_undef],
                                  typ_undef, typ_array tarr))
                | "concat" ->
                    TRef (TArrow (ThisIs (typ_array tarr),
                                  [Env.typ_union env typ_undef 
                                     (typ_array tarr)],
                                  typ_undef, typ_array tarr))
                | s ->
                    error p ("unknown array method " ^ s)
              end
            | _ -> error p 
                ("array index requires Int, got " ^ string_of_typ tidx)
          end
    | TConstr (cname, []), TStrSet [fname] ->
        begin match Env.field_typ env cname fname with
          | Some t -> t
          | None -> begin match cname with 
              | "Undef" 
              | "Null" -> TBot
              | "Num"
              | "Bool"
              | "Int"
              | "Str" -> TRef typ_undef
              | _ -> TRef typ_undef
            end
        end
    | TConstr (cname, []), te ->
          begin match cname with
            | "Undef"
            | "Null" -> TBot
            | "Num"
            | "Bool"
            | "Int"
            | "Str" -> 
                begin match te with 
                  | TConstr ("Int", []) -> TRef (typ_undef)
                  | _ -> error p (sprintf "Can't look up %s[%s]" 
                                    cname (string_of_typ te))
                end
            | c -> match te with
                | TStrMinus strs -> 
                    let flds = Env.class_fields env cname in
                      IdMap.fold (fun k t1 t2 -> 
                                    if (List.mem k strs) then t2 else 
                                      Env.typ_union env t1 t2) flds TBot
                | TConstr ("Int", []) -> TRef (typ_undef)
                | _ -> list_to_typ env (class_types env c)
          end
    | t, TStrSet [s] ->
        error p ("expected object, but got " ^ string_of_typ t ^ 
                   " for lookup of " ^ s)
    | t, f ->
        error p ("field-lookup requires a string literal, got " ^ 
                   (string_of_typ t) ^ "[" ^ 
                   (string_of_typ f) ^ "]")

and tc_exp (env : Env.env) exp = 
  let t = tc_exp_simple env exp in
  Env.normalize_typ env (unfold_typ t)

and tc_def t env def = match def with
    DEnd -> (t, env)
  | DLabel (l, t, d) -> 
      let env' = (Env.bind_lbl l (Env.check_typ dummy env t) env) in
        begin match tc_def None env' d with
          | (Some typ, _) -> if Env.subtype env' typ t then (Some t, env') 
            else
              failwith ("Bad label in DLabel: " ^ (string_of_typ typ) ^ " " ^ (string_of_typ t))
          | _ -> failwith "No type in DLabel"
        end
  | DExp (e, d) -> 
      let t = tc_exp env e in
        if t = TBot then (Some TBot, env)
        else tc_def (Some t) env d
  | DLet (p, x, e1, d2) -> 
      let t = tc_exp env e1 in
        tc_def None (Env.bind_id x t env) d2
  | DRec (binds, d) ->
      let f env (x, t, e) = 
        Env.bind_id x (Env.check_typ (Exp.pos e) env t) env in
      let env = fold_left f env binds in
      let tc_bind (x, t, e) =
        let s = tc_exp env e in
          if Env.subtype env s t then ()
          else (* this should not happen; rec-annotation is a copy of the
                  function's type annotation. *)
            failwith (sprintf "%s is declared to have type %s, but the bound \
                             expression has type %s" x (string_of_typ t)
                        (string_of_typ s)) in
        List.iter tc_bind binds;
        tc_def None env d
  | DConstructor (cexp, d) -> let p = cexp.constr_pos in 
      begin match cexp.constr_typ with
          TArrow (_, arg_typs, rest_typ, result_typ) -> 
            if List.length arg_typs = List.length (cexp.constr_args) then ()
            else raise (
              Typ_error (p,
                         "given " ^ (string_of_int (List.length 
                                                      (cexp.constr_args))) 
                         ^ " arg names but "
                         ^ (string_of_int (List.length arg_typs)) 
                         ^ " arg types"));            
            let bind_arg env x t = Env.bind_id x t env in
            let env = List.fold_left2 bind_arg env (cexp.constr_args) arg_typs
            in
            let env = Env.clear_labels env in           
              begin match result_typ with
                  TObject fields ->
                    (* first update the env to have the class
                       available inside the constructor body *)
                    (* create a new class, add fields as initial methods *)
                    (* also add the constr itself as a tarrow *)
                    let env = Env.new_root_class env cexp.constr_name in
                    let env = Env.bind_id cexp.constr_name 
                      (TRef (Env.check_typ p env cexp.constr_typ)) env in
                    let f (fname, ftype) envacc = Env.add_method
                      cexp.constr_name fname ftype envacc in
                    let env = fold_right f fields env in
                      (* first make sure all fields are initialized with
                         the right types in constr_inits. *)
                    let check_field (name, typ) = match typ with
                      | TRef t -> begin try 
                          let (_, e) = List.find (fun (n,_) -> n = name) 
                            cexp.constr_inits in 
                          let etype = tc_exp env e in
                            if Env.subtype env etype t then () 
                            else 
                              raise 
                                (Typ_error (
                                   p, sprintf "for field %s, expected type %s, \
                                               got %s" name (string_of_typ t)
                                     (string_of_typ etype)))
                        with
                            Not_found -> raise (
                              Typ_error (p, "field " ^ name ^ 
                                           " not initialized in constructor"))
                        end
                      | _ ->
                          raise (Typ_error (p, "expected an arrow type for \
                            the constructor")) in
                      List.iter check_field fields;
                      (* now check the body, with "this" bound to res_type *)
                      ignore (tc_exp (Env.bind_id "this" result_typ env) 
                                cexp.constr_exp);
                      tc_def None env d
                | _ -> raise (Typ_error (
                                p, "constructor's ret type must be obj"))
              end
        | _ -> raise (Typ_error (p, "expected arrow type on constructor"))
      end
  | DExternalMethod (p, cname, mid, me, d) -> 
      begin try
        let expenv = Env.bind_id "this" (TConstr (cname, [])) env in
        let env' = Env.add_method cname mid (TRef (tc_exp expenv me)) env in
          tc_def None env' d
      with Not_found -> raise (
        Typ_error (p, "class " ^ cname ^ " doesnt exist"))
      end
  | DPrototype (p, cname, EObject (p2, props), d) -> 
      begin try
        let expenv = Env.bind_id "this" (TConstr (cname, [])) env in
        let env' = List.fold_right 
          (fun (name, body) env' ->
             Env.add_method cname name (tc_exp expenv body) env')
          props env in
          tc_def None env' d
      with Not_found -> raise (
        Typ_error (p, "class " ^ cname ^ " doesnt exist"))
      end
  | DPrototype (p, _, _, _) -> 
      raise (Typ_error (p, "Must set prototype to an object literal"))

let typecheck init_env defs = 
  let (_, final_env) = tc_def None init_env defs in
  Env.diff final_env init_env


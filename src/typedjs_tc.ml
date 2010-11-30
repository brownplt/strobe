open Prelude
open Typedjs_syntax
open Typedjs_env
open Typedjs_types 
open Format
open Typedjs_dyn
open Typedjs_tc_util

let contracts : (int * typ) IntMap.t ref = ref IntMap.empty

let error_on_unreachable = ref true

let disable_unreachable_check () =
  error_on_unreachable := false

let rec skip n l = if n == 0 then l else (skip (n-1) (List.tl l))
let rec fill n a l = if n <= 0 then l else fill (n-1) a (List.append l [a])

let map_to_list m = 
  IdMap.fold (fun k v l -> (k,v)::l) m []

let error p s = raise (Typ_error (p, s))

let class_fields_list env cnames = 
  List.fold_right (fun cname l ->
                     l@(map_to_list (Env.class_fields env cname)))
    cnames []

let class_types (env : Env.env) constr = IdMapExt.values (Env.class_fields env constr)

let class_types_list env cnames = 
  List.fold_right (fun cname l -> 
                     l@(class_types env cname))
    cnames []
  

let string_of_typ_list ts = 
  fold_left (^) "" (intersperse "," (map string_of_typ ts))

let unfold_typ t = match t with
  | TRec (x, t') -> Typedjs_syntax.Typ.typ_subst x t t'
  | _ -> t

let rec typ_to_list t = match t with
  | TUnion (t1, t2) -> (typ_to_list t1)@(typ_to_list t2)
  | TRec _ -> typ_to_list (unfold_typ t)
  | t -> [t]

let rec list_to_typ env ts = match ts with
  | (t::rest) -> Env.normalize_typ env (TUnion (t, list_to_typ env rest))
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
  | TField -> TField
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
          TUnion (unfold_typ (un_ref t1), unfold_typ (un_ref t2))
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
               (p, sprintf "left-hand side has type %s, but the \
                  right-hand side has type %s"
                  (string_of_typ s) (string_of_typ t)))
      | s, _ -> 
          raise (Typ_error 
                   (p, sprintf "cannot write to LHS (type %s)" 
                      (string_of_typ s)))
    end
  | ELabel (p, l, t, e) -> 
      let t = Env.check_typ p env t in
      let s = tc_exp (Env.bind_lbl l (Env.check_typ p env t) env) e in
        if Env.subtype env s t then t
        else raise (Typ_error (p, sprintf "label type mismatch, expected %s, got %s" (string_of_typ t) (string_of_typ s)))
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
  | EApp (p, f, args) -> begin match applicables env (tc_exp env f) with
      | TForall (x, _, TArrow (obj_typ, expected_typ, rest_typ, result_typ)) ->
          let subst = unify_typ (TArrow (obj_typ, expected_typ, rest_typ, result_typ))
            (TArrow (obj_typ, map (tc_exp env) args, rest_typ, result_typ)) in
            begin try
              let u = IdMap.find x subst in (* TODO: needless recomputation *)
                tc_exp env (EApp (p, ETypApp (p, f, u), args))
            with Not_found ->
              let t = TArrow (obj_typ, expected_typ, rest_typ, result_typ) in
              error p (sprintf "could not determine \'%s in the function type \
                                %s" x (string_of_typ t))
            end
      | TObjStar (_, _, _, TArrow (expected_thist, expected_typs, rest_typ, result_typ))
      | TArrow (expected_thist, expected_typs, rest_typ, result_typ) ->
          let _ = (
            let this_typ = tc_thist env f in
              if not (Env.subtype env this_typ expected_thist) then
                raise (Typ_error 
                         (p, sprintf "expected this type %s, got %s"
                            (string_of_typ expected_thist)
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
                               function expects an argument of type %s" 
                            !arg_ix (string_of_typ arg_typ)
                               (string_of_typ expected_typ)))
            else raise (Typ_error 
                          (p, sprintf "arity-mismatch: the function expects %d \
                                arguments, but %d arguments given."
                             (List.length expected_typs) (List.length args)))
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
      begin match Env.bind_typ env expected_typ with
          (env, TArrow (this_t, arg_typs, rest_typ, result_typ)) ->
            if not (List.length arg_typs = List.length args) then
              error p 
                (sprintf "given %d argument names, but %d argument types"
                   (List.length args) (List.length arg_typs));
            let bind_arg env x t = Env.bind_id x t env in
            let env = List.fold_left2 bind_arg env args arg_typs in
            let env = Env.clear_labels env in
            let env = Env.bind_id "this" this_t env in
            let body_typ = tc_exp env body in
              if Env.subtype env body_typ result_typ then 
                expected_typ
              else raise 
                (Typ_error
                   (p,
                    sprintf "function body has type\n%s\n, but the \
                             return type is\n%s" (string_of_typ body_typ)
                      (string_of_typ result_typ)))
        | _ -> raise (Typ_error (p, "invalid type annotation on a function"))
      end
  | ESubsumption (p, t, e) ->
      let s = tc_exp env e in
      let t = Env.check_typ p env t in
      if Env.subtype env s t then
        t
      else 
        error p (sprintf "%s is not a subtype of %s" 
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
        | e -> error p (sprintf "Not an object literal or new for ObjCast: %s" (Typedjs_syntax.string_of_exp e))
      end in
      let s = tc_exp env e in
      let t = unfold_typ (Env.check_typ p env t) in
        begin match t with
          | TObjStar (fs, proto', other_typ, code) -> 
              if not (Env.subtype env proto proto') then
                error p (sprintf "Mismatched prototypes in ObjCast: \ 
                                 %s" (string_of_typ proto))
              else
                let ok_field fss (k, t) =
                  if List.mem_assoc k fss then
                    Env.subtype env t (List.assoc k fss)
                  else Env.subtype env t other_typ
                in
                  begin match s with 
                    | TObject (fs') -> 
                        if List.for_all (ok_field fs) fs' then t else
                          error p "Invalid ObjCast---bad named fields"
                    | TConstr (cname, []) ->
                        let fs' = map_to_list (Env.class_fields env cname) in
                          if List.for_all (ok_field fs) fs' then t else
                            error p "Invalid ObjCast---constructor missing something"
                    | TConstr ("Array", [tarr]) ->
                        if Env.subtype env tarr other_typ then t else
                          error p (sprintf "Invalid ObjCast---%s </: %s" 
                                    (string_of_typ other_typ) (string_of_typ tarr))
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
  | EForInIdx _ -> TField
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
  match t, field with
    | TObject fs, EConst (_, JavaScript_syntax.CString x) ->
        (try
           let ft = un_ref (snd2 (List.find (fun (x', _) -> x = x') fs)) in
           let vt = tc_exp env newval in
             if Env.subtype env vt ft && Env.subtype env ft vt then t
             else raise (Typ_error (p, (sprintf "%s is not a subtype of %s in \
                                        %s[%s = %s]" (string_of_typ vt)
                                          (string_of_typ ft) (string_of_typ t)
                                          x (string_of_typ ft))))
         with Not_found ->
           raise (Typ_error (p, "the field " ^ x ^ " does not exist")))
    | TObjStar (fs, proto, other_typ, code),
        EConst (_, JavaScript_syntax.CString x) ->
        let vt = tc_exp env newval in
        let other_typ = un_ref other_typ in
          (try
             let ft = un_ref (snd2 (List.find (fun (x', _) -> x = x') fs)) in
               if Env.subtype env vt ft && Env.subtype env ft vt then vt
               else raise (Typ_error (p, (sprintf "%s is not a subtype of %s in \
                                        %s[%s = %s]" (string_of_typ vt)
                                            (string_of_typ ft) (string_of_typ t)
                                        x (string_of_typ ft))))
           with Not_found -> 
             if Env.subtype env vt other_typ && Env.subtype env other_typ vt then vt
             else raise (Typ_error (p, (sprintf "%s is not a subtype of %s in \
                                        %s[%s = %s]" (string_of_typ vt)
                                            (string_of_typ other_typ) (string_of_typ t)
                                        x (string_of_typ other_typ)))))
    | TObjStar (fs, proto, other_typ, code), e ->
	let t_field = tc_exp env e in
        let vt = tc_exp env newval in
        let other_typ = un_ref other_typ in
	  (match t_field with
             | TUnion (TConstr ("Str", []), TConstr ("Undef", []))
             | TConstr ("Str", []) -> 
                 let fts = map un_ref (map snd fs) in
                   if List.for_all (fun t -> Env.subtype env vt t 
                                      && Env.subtype env t vt) fts then
                     vt
                   else
                     raise (Typ_error (p, (sprintf "Dictionary assignment error: %s"
                                             (string_of_typ vt))))
             | TConstr ("Int", []) ->
                 if Env.subtype env vt other_typ && Env.subtype env other_typ vt
                 then vt else 
                   raise (Typ_error (p, (sprintf "Dictionary assignment error: %s"
                                           (string_of_typ vt))))
	     | t -> raise (Typ_error (p, (sprintf "Index was type %s in  \
                                       dictionary assignment\n" (string_of_typ t)))))
    | TConstr ("Array", [tarr]), eidx ->
        let tidx = tc_exp env eidx in
        let vt = tc_exp env newval in
        let tarr = un_ref tarr in
          (match tidx with
             | TConstr ("Int", []) -> 
                 if Env.subtype env vt tarr && Env.subtype env tarr vt then
                   vt
                 else
                  raise (Typ_error (p, "Bad array type"))
             | _ -> raise (Typ_error (p, "Array index must be int")))
    | TConstr (cname, _), EConst (_, JavaScript_syntax.CString x) ->
        let vt = tc_exp env newval in
          begin match Env.field_typ env cname x with
            | Some t -> 
                let ft = un_ref t in
                  if Env.subtype env vt ft then
                    vt
                  else
                    raise (Typ_error (p, (sprintf "Bad assignment to \
                                      %s[%s:%s = %s]" cname x 
                                         (string_of_typ ft)
                                         (string_of_typ vt))))
            | None -> begin match cname with 
                | "Undef" 
                | "Null" -> TBot (* Actually errors *)
                | "Num"
                | "Bool"
                | "Int"
                | "Str" -> vt
                | _ -> TConstr ("Undef", [])
              end
          end
    | _ -> raise (Typ_error (p, "No object update on non-objects yet"))

and bracket p env ft ot =
  match ot, ft with
    | TStrSet strs, _
    | TStrMinus strs, _ -> bracket p env ft (TConstr ("Str", []))
    | TObject fs, TStrSet [x] ->
        (try
           snd2 (List.find (fun (x', _) -> x = x') fs)
         with Not_found ->
           raise (Typ_error (p, "the field " ^ x ^ " does not exist")))
    | TObjStar (fs, proto, other, code), _ ->
        (match ft with
           | TConstr ("Str", []) 
           | TUnion (TConstr ("Str", []), TConstr ("Undef", [])) ->
               let rem_names = TStrMinus (map fst fs) in
               let field_typ = list_to_typ env (map snd fs) in
                 (TUnion (Env.check_typ p env field_typ,
                          TUnion (bracket p env rem_names proto,
                                  other)))
           | TStrMinus strs -> 
               let flds = (List.filter (fun fld ->
                                          not (List.mem (fst fld) strs))
                             fs) in
               let field_typ = list_to_typ env (map snd flds) in
               let rem_ft = TStrMinus (strs@(map fst flds)) in
                 (TUnion (Env.check_typ p env field_typ,
                          TUnion (bracket p env rem_ft proto,
                                  other)))
           | TStrSet [x] ->
	       (try
	          snd2 (List.find (fun (x', _) -> x = x') fs)
	        with Not_found ->
	          (TUnion (TRef (unfold_typ (un_ref other)), 
                           TUnion(TConstr ("Undef", []),
                                  bracket p env ft proto))))
           | TConstr ("Int", []) ->
               TRef (TUnion (unfold_typ (un_ref other),
                             TConstr ("Undef", [])))
           | t -> error p (sprintf "Index was type %s in \
                                    dictionary lookup" (string_of_typ t)))
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
                    TRef (TArrow (TConstr ("Array", [tarr]),
                                  [un_ref tarr], typ_undef, typ_undef))
                | "join" ->
                    (match un_ref tarr with
                       | TConstr ("Str", []) ->
                           TRef (TArrow (TConstr ("Array", [tarr]),
                                         [typ_str], typ_undef, typ_str))
                       | _ -> error p ("expected array of strings"))
                | "slice" ->
                    TRef (TArrow (TConstr ("Array", [tarr]),
                                  [TConstr ("Int", []);
                                   TUnion (TConstr ("Int", []), TConstr ("Undef", []))], 
                                  typ_undef, TConstr ("Array", [tarr])))
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
              | "Null" -> TConstr ("Undef", [])
              | "Num"
              | "Bool"
              | "Int"
              | "Str" -> TConstr ("Undef", [])
              | _ -> TConstr ("Undef", [])
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
                  | TConstr ("Int", []) -> TRef (TConstr ("Undef", []))
                  | _ -> error p (sprintf "Can't look up %s[%s]" 
                                    cname (string_of_typ te))
                end
            | c -> list_to_typ env (class_types env c)
          end
    | TField, tf -> begin match tf with
        | TField -> TField
        | _ -> error p "expected a TField index"
      end
    | t, TStrSet [s] ->
        error p ("expected object, but got " ^ string_of_typ t ^ " for lookup of " ^ s)
    | t, f ->
        error p ("field-lookup requires a string literal, got " ^ 
                   (string_of_typ t) ^ "[" ^ 
                   (string_of_typ f) ^ "]")

and tc_exp (env : Env.env) exp = Env.normalize_typ env (unfold_typ (tc_exp_simple env exp))

let rec tc_def env def = match def with
    DEnd -> env
  | DExp (e, d) -> 
      let _ = tc_exp env e in
        tc_def env d
  | DLet (p, x, e1, d2) -> tc_def (Env.bind_id x (tc_exp env e1) env) d2
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
        tc_def env d
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
                      tc_def env d
                | _ -> raise (Typ_error (
                                p, "constructor's ret type must be obj"))
              end
        | _ -> raise (Typ_error (p, "expected arrow type on constructor"))
      end
  | DExternalMethod (p, cname, mid, me, d) -> 
      try
        let expenv = Env.bind_id "this" (TConstr (cname, [])) env in
        let env' = Env.add_method cname mid (TRef (tc_exp expenv me)) env in
          tc_def env' d
      with Not_found -> raise (
        Typ_error (p, "class " ^ cname ^ " doesnt exist"))
        
let typecheck init_env defs = 
  let final_env = tc_def init_env defs in
  Env.diff final_env init_env


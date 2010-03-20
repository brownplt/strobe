open Prelude
open Typedjs_syntax
open Exprjs_syntax
open Typedjs_types
open Typedjs_env

module H = Hashtbl

exception Not_well_formed of pos * string

let type_from_comment ((pos, end_p), str) =
  let lexbuf = Lexing.from_string str in
    lexbuf.Lexing.lex_start_p <- pos;
    lexbuf.Lexing.lex_curr_p <- pos;
    try
      Typedjs_parser.typ_ann Typedjs_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "lexical error parsing type at %s"
                       (string_of_position
                          (lexbuf.Lexing.lex_curr_p, lexbuf.Lexing.lex_curr_p)))
      |  Typedjs_parser.Error ->
           failwith (sprintf "parse error parsing type at %s"
                       (string_of_position
                          (lexbuf.Lexing.lex_curr_p, lexbuf.Lexing.lex_curr_p)))

let func_index = ref 0

let type_dbs : annotation PosMap.t ref = ref PosMap.empty

let inferred_array : annotation array ref = ref (Array.make 0 (ATyp TBot))

let hints : (pos, annotation) H.t = H.create 200

let rec init_types lst = match lst with
    [] -> ()
  | (pos, str) :: rest when String.length str > 0 && str.[0] == ':' ->
      begin match type_from_comment (pos, str) with
        | AInferred xs -> 
            inferred_array := Array.of_list xs
        | AUpcast t -> H.add hints pos (AUpcast t)
        | x ->
            type_dbs := PosMap.add pos x !type_dbs
      end;
      init_types rest
  | _ :: rest -> init_types rest

let annotation_between start_p end_p =
  let found = ref None in
  let f (typ_start_p, typ_end_p) ann = 
    if typ_start_p.Lexing.pos_cnum > start_p.Lexing.pos_cnum &&
      typ_end_p.Lexing.pos_cnum < end_p.Lexing.pos_cnum then
        begin match !found with
           None -> found := Some ((typ_start_p, typ_end_p), ann)
          | Some _ ->
              raise (Not_well_formed ((start_p, end_p), 
                                      "found multiple type annotations"))
        end in
    PosMap.iter f !type_dbs;
    !found

let constr_between (p1, p2) = match annotation_between p1 p2 with
    Some (p, AConstructor typ) -> 
      type_dbs := PosMap.remove p !type_dbs;
      Some typ
  | _ -> None

let type_between (start_p : Lexing.position) (end_p : Lexing.position) : typ =
  match annotation_between start_p end_p with
      Some (pos, ATyp typ) -> 
        type_dbs := PosMap.remove pos !type_dbs;
        typ
    | None when !func_index < Array.length !inferred_array ->
        begin match !inferred_array.(!func_index) with
            ATyp t -> t
          | _ -> raise (Not_well_formed 
                          ((start_p, end_p), "expected a type annotation"))
        end
        
    | _ ->
        raise (Not_well_formed ((start_p, end_p), "expected a type annotation"))

(******************************************************************************)


(** [seq expr] returns the [VarDeclExpr]s at the head of a sequence of
    expressions. Assumes that [SeqExpr]s are nested on the right.

    We also transform [FuncStmtExpr]s into [VarDeclExprs]. In
    JavaScript, function statements are lifted to the top of the
    function, but Typed JavaScript's scope rules prevent them from being
    used before they are defined in the program text. *)
let rec seq expr = match expr with
    SeqExpr (a1, VarDeclExpr (a2, x, e1), e2) ->
      let (decls, body) = seq e2 in
        ((a2, x, e1) :: decls, body)
  | SeqExpr (a1, FuncStmtExpr (a2, f, args, e1), e2) ->
      let (decls, body) = seq e2 in
        ((a2, f, FuncExpr (a2, args, e1)) :: decls, body)
  | _ -> ([], expr)

let is_func_decl (_, _, e) = match e with
    FuncExpr _ -> true
  | _ -> false

let is_not_func_decl d = not (is_func_decl d)

let is_empty lst = match lst with
    [] -> true
  | _ -> false

type env = bool IdMap.t

let rec exp (env : env) expr = match expr with
    ConstExpr (a, c) -> EConst (a, c)
  | ArrayExpr (a, es) -> EArray (a, map (exp env) es)
  | ObjectExpr (a, ps) -> 
      if List.length ps != List.length (nub (map (fun (_, p, _) -> p) ps)) then
        raise (Not_well_formed (a, "repeated field names"));
      ERef (a, EObject (a, map (fun (_, x, e) ->  x, exp env e) ps))
  | ThisExpr a -> EThis a
  | VarExpr (a, x) -> begin
      try
        if IdMap.find x env then
          EDeref (a, EId (a, x))
        else
          EId (a, x)
      with Not_found -> raise (Not_well_formed (a, x ^ " is not defined"))
    end
  | BracketExpr (a, e1, e2) -> EBracket (a, EDeref (a, exp env e1), exp env e2)
  | NewExpr (a, c, args) -> ENew (a, exp env c, map (exp env) args)
  | PrefixExpr (a, op, e) -> EPrefixOp (a, op, exp env e)
  | InfixExpr (a, op, e1, e2) -> EInfixOp (a, op, exp env e1, exp env e2)
  | IfExpr (a, e1, e2, e3) -> EIf (a, exp env e1, exp env e2, exp env e3)
  | AssignExpr (a, VarLValue (p', x), e) -> ESetRef (a, EId (p', x), exp env e)
  | AssignExpr (p, PropLValue (_, e1, e2), e3) ->
      ELet 
        (p, "%obj", exp env e1,
         ESetRef
           (p, EId (p, "%obj"),
            EUpdateField
              (p, EDeref (p, EId (p, "%obj")), exp env e2, exp env e3)))
  | AppExpr (a, f, args) -> EApp (a, exp env f, map (exp env) args)
  | FuncExpr _ -> 
      (match match_func env expr with
           Some (_, e) -> e
         | None -> failwith "match_func returned None on a FuncExpr (1)")
  | LetExpr (a, x, e1, e2) ->
      ELet (a, x, exp env e1, exp (IdMap.add x true env) e2)
  | TryCatchExpr (a, body, x, catch) ->
      ETryCatch (a, exp env body, x, exp env catch)
  | TryFinallyExpr (a, body, finally) -> 
      ETryFinally (a, exp env body, exp env finally)
  | ThrowExpr (a, e) -> EThrow (a, exp env e)
  | WhileExpr (a, e1, e2) ->
      let loop_typ = TArrow (TTop, [], typ_undef) in
        ERec ([("%loop", loop_typ,
                EFunc (a, [], loop_typ,
                       EIf (a, exp env e1, 
                            ESeq (a, exp env e2, 
                                  EApp (a, EId (a, "%loop"), [])),
                            EConst (a, CUndefined))))],
              EApp (a, EId (a, "%loop"), []))
  | DoWhileExpr (a, e1, e2) ->
      let loop_typ = TArrow (TTop, [], typ_undef) in
        ERec ([("%loop", loop_typ,
                EFunc (a, [], loop_typ,
                       ESeq (a, exp env e1, 
                             EIf (a, exp env e2, 
                                  EApp (a, EId (a, "%loop"), []),
                                    EConst (a, CUndefined)))))],
              EApp (a, EId (a, "%loop"), []))

  | LabelledExpr (a, x, e) -> 
      (** We assume that this [LabelledExpr] is from a [LabelledStmt]. 
          Therefore, the return type is [ty_undef]. *)
      ELabel (a, x, typ_undef, exp env e)
  | BreakExpr (a, x, e) -> EBreak (a, x, exp env e)
  | SeqExpr (a, _, _) -> block_intro env (seq expr)
  | VarDeclExpr (a, x, e) -> 
      (* peculiar code: body or block ends with var *)
      ELet (a, x, exp env e, EConst (a, CUndefined))
  | FuncStmtExpr (a, f, args, body) ->
      begin match match_func (IdMap.add f false env) 
        (FuncExpr (a, args, body)) with
            Some (t, e) ->
              ERec ([ (f,  t, e) ], EConst (a, CUndefined))
          | None -> failwith "match_func returned None on a FuncExpr (2)"
      end
  | ParenExpr (p, e) -> EParens (p, exp env e)

and match_func env expr = match expr with
  | FuncExpr (a, args, LabelledExpr (a', "%return", body)) ->
      if List.length args != List.length (nub args) then
        raise (Not_well_formed (a, "each argument must have a distinct name"));
      let typ = type_between (fst2 a) (fst2 a') in
        (* Within [body], a free variable, [x] cannot be referenced, if there
           is any local variable with the name [x]. *)
      let locally_defined_vars = locals body in
      let visible_free_vars = 
        IdSet.diff (IdSetExt.from_list (IdMapExt.keys env))
          locally_defined_vars in
      let lambda_bound_vars = IdSetExt.from_list args in
      let locally_shadowed_args = 
        IdSet.inter lambda_bound_vars locally_defined_vars in
        if not (IdSet.is_empty locally_shadowed_args) then
          begin
            IdSetExt.pretty Format.str_formatter Format.pp_print_string 
              locally_shadowed_args;
            raise (Not_well_formed 
                     (a, "the following arguments are redefined as local \
                          variables: " ^ Format.flush_str_formatter ()))
          end;
        let env' = 
          IdMap.fold (fun key v acc ->
                        if IdSet.mem key visible_free_vars then
                          IdMap.add key v acc
                        else 
                          acc)
            env IdMap.empty in
        let env' = 
          fold_left (fun acc x -> IdMap.add x true acc) env' args in
        let mutable_arg exp id =
          ELet (a, id, ERef (a, EId (a, id)), exp) in
        begin match typ with
            TArrow (_, arg_typs, r) ->
              incr func_index;
              if List.length args != List.length arg_typs then
                raise (Not_well_formed (a, "not all arguments have names"));
              Some (typ, EFunc (a, args, typ, 
                                fold_left mutable_arg
                                  (ELabel (a', "%return", r, exp env' body))
                                  args))
          | _ ->
              raise (Not_well_formed (a, "expected a function type"))
        end
  | FuncExpr (a, _, _) ->
      failwith ("expected a LabelledExpr at " ^ string_of_position a)
  | _ -> None


and exp_seq env e = match e with
    SeqExpr (a, e1, e2) -> ESeq (a, exp env e1, exp env e2)
  | _ -> exp env e

and block_intro env (decls, body) = match take_while is_func_decl decls with
    [], [] -> exp_seq env body
  | [], (a, x, e) :: rest ->
      ELet (a, x, ERef (a, exp env e),
            block_intro (IdMap.add x true env) (rest, body))
  | funcs, rest ->
      let new_ids = map snd3 funcs in
      let env' = fold_left (fun acc f -> IdMap.add f false acc)  env new_ids in
      let mk_bind (_, x, expr) = 
        let e = exp env' expr in
          (match e with
               EFunc (_, _, t, _) -> (x, t, e)
             | _ -> failwith "expected a FuncExpr") in
        ERec (map mk_bind funcs,
              block_intro env' (rest, body))

(* assumes [SeqExpr]s are nested to the right. *)
let rec flatten_seq (expr : expr) : expr list = match expr with
    SeqExpr (_, e1, e2) -> e1 :: flatten_seq e2
  | _ -> [ expr ]

let match_constr_body env expr = match expr with
    (* we drop the LabelledExpr, which will prevents us from using
        return within a constructor. *)
    FuncStmtExpr (p, f, args, LabelledExpr (p', l, body)) ->
      begin match constr_between (fst2 p, fst2 p') with
          None -> None
        | Some typ -> 
            let locally_defined_vars = locals body in
            let visible_free_vars = 
              IdSet.diff (IdSetExt.from_list (IdMapExt.keys env))
                locally_defined_vars in
            let lambda_bound_vars = IdSetExt.from_list args in
            let locally_shadowed_args = 
              IdSet.inter lambda_bound_vars locally_defined_vars in
              if not (IdSet.is_empty locally_shadowed_args) then
                begin
                  IdSetExt.pretty Format.str_formatter Format.pp_print_string 
                    locally_shadowed_args;
                  raise (Not_well_formed 
                           (p, "the following arguments are redefined as local \
                          variables: " ^ Format.flush_str_formatter ()))
                end;
              let env' = 
                IdMap.fold (fun key v acc ->
                              if IdSet.mem key visible_free_vars then
                                IdMap.add key v acc
                              else 
                                acc)
                  env IdMap.empty in
                (* when doing inits, nothing is reffed yet *)
              let inits_env = 
                fold_left (fun acc x -> IdMap.add x false acc) env' args in
              let env' = 
                fold_left (fun acc x -> IdMap.add x true acc) env' args in
                (* take the beginning "this.foo = bar"s and put them into inits
                *)
              let match_init eexp = match eexp with
                  AssignExpr (_,
                              PropLValue (
                                _, ThisExpr _, ConstExpr (_, CString fname)),
                              fval) -> Some (fname, exp inits_env fval)
                | _ -> None in                
              let body_exp = ref body in
              let inits : (id * exp) list ref = ref [] in
              let rec proc_body e = match e with
                  SeqExpr (_, e1, e2) -> begin match match_init e1 with
                      Some init -> begin
                        inits := !inits @ [ init ];
                        body_exp := e2;
                        proc_body e2;
                      end
                    | None -> () 
                  end
                | _ -> () in
                proc_body !body_exp;
                (* turn the arguments into mutable ones *)
                let mutable_arg exp id = 
                  ELet (p, id, ERef (p, EId (p, id)), exp) in
                  Some 
                    { constr_pos = p;
                      constr_name = f;
                      constr_typ = typ;
                      constr_args = args;
                      constr_inits = !inits;
                      constr_exp = fold_left mutable_arg 
                        (exp env' !body_exp) args;
                      constr_prototype = EObject (p, [])
                    }
      end 
  | _ -> None


(** [match_func_decl expr] matches function statements and variables bound to
    function expressions. *)
let match_func_decl expr = match expr with
    VarDeclExpr (p, x, e) ->
      (match e with
           FuncExpr _ -> Some (p, x, e)
         | _ -> None)
  | FuncStmtExpr (p, f, args, e) -> Some (p, f, FuncExpr (p, args, e))
  | _ -> None

let match_decl expr = match expr with
    VarDeclExpr (p, x, e) -> Some (p, x, e)
  | _ -> None

let match_external_method expr = match expr with
    AssignExpr (
      p, 
      PropLValue (
        _, 
        BracketExpr (
          _, VarExpr (_, cname), ConstExpr (_, CString "prototype")),
        ConstExpr (_, CString methodname)),
      methodexpr) -> Some (p, cname, methodname, methodexpr)
  | _ -> None


let rec defs env lst = 
  begin match lst with
      [] -> DEnd
    | expr :: lst' ->
        begin match match_constr_body env expr with
          | Some c -> 
              DConstructor (c, defs (IdMap.add c.constr_name false env) lst')
          | None ->
              begin match match_while match_func_decl (expr :: lst') with
                  [], expr :: lst' -> begin match match_decl expr with
                      None -> begin match match_external_method expr with
                          None -> DExp (exp env expr, defs env lst')
                        | Some (p, name, mid, me) -> 
                            DExternalMethod (p, name, mid, exp env me, 
                                             defs env lst')
                      end
                    | Some (p, x, expr) -> 
                        let env' = IdMap.add x true env in
                          DLet (p, x, ERef (p, exp env expr), defs env' lst')
                  end
                | func_binds, lst' ->
                    let mk acc (_, f, _) = IdMap.add f false acc in
                    let env' = fold_left mk env func_binds in
                    let mk_bind (_, x, expr) = match match_func env' expr with
                        Some (t, e) -> (x, t, e)
                      | None -> failwith "match_func returned None (defs)" in
                      DRec (map mk_bind func_binds, defs env' lst')
              end
        end
  end

(*

let match_external_method binds expr = match expr with
    AssignExpr 
      (p,
       PropLValue 
         (_, BracketExpr (_, VarExpr (_, c_name), StringExpr (_, "prototype")),
          StringExpr (_, f_name)),
       rhs_expr) -> begin match match_func binds rhs_expr with
          Some (typ, e) -> Some (p, c_name, f_name, typ, e)
        | None -> None
      end
  | _ -> None

let match_methods_for c_name env expr = match expr with
    AssignExpr 
      (p,
       PropLValue 
         (_, BracketExpr (_, VarExpr (_, c_name'), StringExpr (_, "prototype")),
          StringExpr (_, f_name)),
       rhs_expr) when c_name = c_name' -> Some (p, f_name, exp env rhs_expr)
  | _ -> None
  
let def binds expr = match expr with
    AssignExpr 
      (p,
       PropLValue 
         (_, BracketExpr (_, VarExpr (_, c_name), StringExpr (_, "prototype")),
          StringExpr (_, f_name)),
       rhs_expr) ->
        DExternalField (p, c_name, f_name, exp binds rhs_expr), binds
  | VarDeclExpr (_, x, _) -> DExp (exp binds expr), IdSet.add x binds
  | _ -> DExp (exp binds expr), binds


let match_prototype c_name env lst =
  match match_while (match_methods_for c_name env) lst with
      [], expr :: lst' ->
        begin match expr with
            AssignExpr
              (p,
               PropLValue
                 (_, VarExpr (_, c_name'), StringExpr (_, "prototype")),
               rhs_expr) when c_name = c_name' ->
                Some (exp env rhs_expr), lst'
          | _ -> None, lst
        end
    | (p, f_name, f_exp) :: fields, lst' -> 
        let mk_field (_, x, e) = (x, false, e) in
          Some (EObject (p, map mk_field ((p, f_name, f_exp) :: fields))), lst'
      

let rec defs binds lst  = 
  match match_while (match_external_method binds) lst with
    | [], [] -> []
    | [],  e :: lst'  ->
        let d, binds' = def binds e in
          d :: defs binds' lst'
    | exts, lst ->
        DExternalMethods exts :: defs binds lst 
*)


module InsertHints = struct

  let rec ins_list (p : pos) (v : exp -> exp) lst  : exp list option =
    match lst with
      | [] -> None
      | e :: rest -> 
          if Pos.before (Exp.pos e) p then
            begin match ins_list p v rest with
              | None -> None
              | Some rest' -> Some (e :: rest')
            end
          else 
            begin match ins p v e with
              | Some e' -> Some (e' :: rest)
              | None -> None
            end

  and ins (p : pos) (v : exp -> exp) (exp : exp) : exp option = 
    begin match exp with
      | EParens (q, e) when Pos.before p q -> Some (v exp)
      | _ -> 
          let lst = Exp.children exp in
            match ins_list p v lst with
              | Some lst' -> Some (Exp.set_children exp lst')
              | None -> 
                  if Pos.before p (Exp.pos exp) then
                    Some (v exp)
                  else
                    None
    end

  let mk p a exp = match a with
    | AUpcast typ -> 
        ins p (fun e -> ESubsumption (p, typ, e)) exp
    | _ -> failwith "expected Upcast"

  let rec insert_hint p a def = match def with
    | DLet (q, x, e, d) -> begin match mk p a e with
        | Some e' -> DLet (q, x, e', d)
        | None -> DLet (q, x, e, insert_hint p a d)
      end
    | DRec (binds, d) -> begin match insert_bind p a binds with
        | Some binds' -> DRec (binds', d)
        | None -> DRec (binds, insert_hint p a d)
      end
    | DExp (e, d) -> begin match mk p a e with
        | Some e' -> DExp (e', d)
        | None -> DExp (e, insert_hint p a d)
      end
    | DConstructor (constr, d) -> begin match mk p a constr.constr_exp with
        | Some e' ->
            (* TODO: We ignore hints on initializers. *)
            DConstructor ({ constr with constr_exp = e' }, d)
        | None -> DConstructor (constr, insert_hint p a d)
      end
    | DExternalMethod (p, x, y, e, d) -> begin match mk p a e with
        | Some e' -> DExternalMethod (p, x, y, e', d)
        | None -> DExternalMethod (p, x, y, e, insert_hint p a d)
      end
    | DEnd -> raise (Not_well_formed (p, "unusable typing hint"))

  and insert_bind p a binds = match binds with
    | [] -> None
    | (x, t, e) :: rest -> match mk p a e with
        | Some e' -> Some ((x, t, e') :: rest)
        | None -> match insert_bind p a rest with
            | Some rest' -> Some ((x, t, e) :: rest')
            | None -> None

end

let from_exprjs env expr = 
  let exp = defs 
    (IdSet.fold (fun x env -> IdMap.add x false env) (Env.dom env) IdMap.empty)
    (flatten_seq expr) in
  let exp = H.fold InsertHints.insert_hint hints exp in
    exp

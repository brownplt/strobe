open Prelude
open Typedjs_syntax
open Exprjs_syntax
open Typedjs_env
open Typedjs_tc_util

module H = Hashtbl
module S = JavaScript_syntax

exception Not_well_formed of pos * string

let error (p : pos) (s : string) : 'exn =
  raise (Not_well_formed (p, s))

let parse_annotation (pos, end_p) str =
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
  | SeqExpr (a1, HintExpr (a3, txt, FuncStmtExpr (a2, f, args, e1)), e2) ->
      let (decls, body) = seq e2 in
        ((a2, f, HintExpr (a3, txt, FuncExpr (a2, args, e1))) :: decls, body)
  | _ -> ([], expr)

let rec is_value e = match e with
  | HintExpr (_, _, FuncExpr _) -> true
  | ConstExpr _ -> true
  | ObjectExpr (_, flds) -> List.for_all (fun (_, _, e') -> is_value e') flds
  | _ -> false

let is_func_decl (_, _, e) = is_value e

let is_empty lst = match lst with
    [] -> true
  | _ -> false

type env = bool IdMap.t

let to_string  p e = EPrefixOp (p, "%ToString",  e)
let to_object  p e = EPrefixOp (p, "%ToObject",  e)
let to_boolean p e = EPrefixOp (p, "%ToBoolean", e)

let rec exp (env : env) expr = match expr with
  | ConstExpr (a, c) -> EConst (a, c)
  | HintExpr (p, txt, ArrayExpr (p', [])) -> 
      begin match parse_annotation p txt with
        | ATyp t -> EEmptyArray (p, TRef t)
        | _ -> 
            raise (Not_well_formed (p, "expected the type of array elements" ))
      end
  | ArrayExpr (a, es) -> EArray (
      a, map (fun e -> ERef (a, RefCell, exp env e)) es)
  | ObjectExpr (a, ps) -> 
      if List.length ps != List.length (nub (map (fun (_, p, _) -> p) ps)) then
        raise (Not_well_formed (a, "repeated field names"));
      ERef (a, RefCell, EObject (a, map (fun (_, x, e) ->  x, exp env e) ps))
  | ThisExpr a -> EId (a, "this")
  | VarExpr (a, x) -> begin try
      if IdMap.find x env then
        EDeref (a, EId (a, x))
      else
        EId (a, x)
    with Not_found -> error a (x ^ " is not defined")
    end
  | IdExpr (a, x) -> EId (a, x)
  | BracketExpr (a, e1, e2) -> 
      EBracket (a, EDeref (a, to_object a (exp env e1)),
                to_string a (exp env e2))
  | NewExpr (a, VarExpr (_, x), args) -> ENew (a, x, map (exp env) args)
  | NewExpr (p, _, _) ->
      raise (Not_well_formed (p, "new expressions much name the constructor"))
  | PrefixExpr (a, op, e) -> EPrefixOp (a, op, exp env e)
  | InfixExpr (p, "&&", e1, e2) -> 
      EIf (p, exp env e1, exp env e2, EConst (p, S.CBool false))
  | InfixExpr (p, "||", e1, e2) ->
      ELet (p, "%or-left", exp env e1,
            EIf (p, EId (p, "%or-left"),
                 EId (p, "%or-left"),
                 exp env e2))
  | InfixExpr (a, op, e1, e2) -> EInfixOp (a, op, exp env e1, exp env e2)
  | IfExpr (a, e1, e2, e3) -> 
      EIf (a, to_boolean a (exp env e1), exp env e2, exp env e3)
  | AssignExpr (a, VarLValue (p', x), e) -> ESetRef (a, EId (p', x), exp env e)
  | AssignExpr (p, PropLValue (_, e1, e2), e3) ->
      ELet (p, "%obj", to_object p (exp env e1),
            ESetRef(p, EId (p, "%obj"),
              EUpdate (p, EDeref (p, EId (p, "%obj")), 
                       to_string p (exp env e2), 
                       exp env e3)))
  | AppExpr (a, ((BracketExpr (a2, e1, e2)) as f), args) ->
      EApp (a, exp env f, (exp env e1)::(map (exp env) args))
  | AppExpr (a, f, args) -> 
      EApp (a, exp env f, EDeref (a, EId (a, "%global"))::(map (exp env) args))
  | LetExpr (a, x, e1, e2) ->
      ELet (a, x, exp env e1, exp (IdMap.add x true env) e2)
  | TryCatchExpr (a, body, x, catch) ->
      ETryCatch (a, exp env body, x, exp env catch)
  | TryFinallyExpr (a, body, finally) -> 
      ETryFinally (a, exp env body, exp env finally)
  | ThrowExpr (a, e) -> EThrow (a, exp env e)
  | WhileExpr (a, e1, e2) ->
      let loop_typ = TArrow ([], TPrim Undef) in
        ERec ([("%loop", loop_typ,
                EFunc (a, [], loop_typ,
                       EIf (a, exp env e1, 
                            ESeq (a, exp env e2, 
                                  EApp (a, EId (a, "%loop"), [])),
                            EConst (a, S.CUndefined))))],
              EApp (a, EId (a, "%loop"), []))
  | DoWhileExpr (a, body_e, test_e) ->
      let loop_typ = TArrow ([], TPrim Undef) in
        ERec ([("%loop", loop_typ,
                EFunc (a, [], loop_typ,
                       ESeq (a, exp env body_e, 
                             EIf (a, exp env test_e, 
                                  EApp (a, EId (a, "%loop"), []),
                                    EConst (a, S.CUndefined)))))],
              EApp (a, EId (a, "%loop"), []))

  | LabelledExpr (a, x, e) -> 
      (** We assume that this [LabelledExpr] is from a [LabelledStmt]. 
          Therefore, the return type is [ty_undef]. *)
      ELabel (a, x, TPrim Undef, exp env e)
  | BreakExpr (a, x, e) -> EBreak (a, x, exp env e)
  | SeqExpr 
      (p,
       IfExpr (_, IdExpr _, ConstExpr (_, S.CUndefined), 
               ConstExpr (_, S.CUndefined)),
       next_expr) -> exp env next_expr
  | SeqExpr (a, _, _) -> block_intro env (seq expr)
  | VarDeclExpr (a, x, e) -> 
      (* peculiar code: body or block ends with var *)
      ELet (a, x, exp env e, EConst (a, S.CUndefined))
  | HintExpr (p', txt, FuncStmtExpr (a, f, args, body)) ->
      begin match match_func (IdMap.add f false env) 
        (HintExpr (p', txt, FuncExpr (a, args, body))) with
            Some (t, e) ->
              ERec ([ (f,  t, e) ], EConst (a, S.CUndefined))
          | None -> failwith "match_func returned None on a FuncExpr (2)"
      end
  | HintExpr (p, txt, FuncExpr _) -> 
      (match match_func env expr with
           Some (_, e) -> e
         | None -> failwith "match_func returned None on a FuncExpr (1)")
  | HintExpr (p, text, e) ->
      let e' = exp env e in
        begin match parse_annotation p text with
	  | ACheat typ -> ECheat (p, typ, e')
          | AUpcast typ -> ESubsumption (p, typ, e')
          | ADowncast typ -> EDowncast (p, typ, e')
          | ATypAbs (x, t) -> ETypAbs (p, x, t, e')
          | ATypApp t -> ETypApp (p, e', t)
          | AAssertTyp t -> EAssertTyp (p, t, e')
          | _ -> error p "unexpected hint"
        end
  | FuncExpr (p, _, _) -> 
      raise (Not_well_formed (p, "function is missing a type annotation"))
  | FuncStmtExpr (p, _, _, _) ->
      raise (Not_well_formed (p, "function is missing a type annotation"))
  | ForInExpr (p, x, objExpr, body) -> begin match exp env objExpr with
      | EDeref (_, EId (_, obj)) ->
          let loop_typ = TArrow ([TField; TField], TPrim Undef) in
            ERec ([("%loop", loop_typ,
                    EFunc (p, [obj; x], loop_typ,
                           ESeq (p, exp env body, 
                                 EApp (p, EId (p, "%loop"),
                                       [ EId (p, obj); EId (p, x) ]))))],
                  EApp (p, EId (p, "%loop"), [EForInIdx p; EForInIdx p]))
      | _ ->
          raise (Not_well_formed (p, "for-in loops require a named object"))
    end



and match_func env expr = match expr with
  | HintExpr (p, txt, FuncExpr (a, args, LabelledExpr (a', "%return", body))) ->
      if List.length args != List.length (nub args) then
        raise (Not_well_formed (a, "each argument must have a distinct name"));
      let typ = match parse_annotation p txt with
        | ATyp t -> t
        | _ -> raise
            (Not_well_formed (p, "expected type on function, got " ^ txt)) in
      let locally_defined_vars = locals body in
      let visible_free_vars = 
        IdSet.diff (IdSetExt.from_list (IdMapExt.keys env))
          locally_defined_vars in
      let lambda_bound_vars = IdSetExt.from_list args in
      let locally_shadowed_args = 
        IdSet.inter lambda_bound_vars locally_defined_vars in
        if not (IdSet.is_empty locally_shadowed_args) then
          begin
            IdSetExt.p_set FormatExt.text
              locally_shadowed_args Format.str_formatter;
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
          ELet (a, id, ERef (a, RefCell, EId (a, id)), exp) in
        let args = "this"::args in
        begin match Typ.match_func_typ typ with
            Some (arg_typs, r) ->
              if List.length args != List.length arg_typs then
                raise (Not_well_formed (
                         a, sprintf "given %d args but %d arg types"
                           (List.length args) (List.length arg_typs)));
              Some (typ, EFunc (a, args, typ, 
                                fold_left mutable_arg
                                  (ELabel (a', "%return", r, exp env' body))
                                  args))
          | None -> raise (Not_well_formed (a, "expected a function type"))
        end
  | FuncExpr (a, _, _) ->
      failwith ("expected a LabelledExpr at " ^ string_of_position a)
  | _ -> None

and exp_seq env e = match e with
    SeqExpr (a, e1, e2) -> ESeq (a, exp env e1, exp env e2)
  | _ -> exp env e

and func_exp env (_, x, expr) = 
  let e = exp env expr in
    (x, typ_of_value e, e)

and bind_func_ref (x, t, e) rest_exp = 
  let p = Exp.pos e in
    ELet (p, x, ERef (p, RefCell, ESubsumption (p, t, EBot p)), rest_exp)

and set_func_ref (x, t, e) rest_exp =
  let p = Exp.pos e in
    ESeq (p, ESetRef (p, EId (p, x), e), rest_exp)
  
and block_intro env (decls, body) = match take_while is_func_decl decls with
    [], [] -> exp_seq env body
  | [], (a, x, e) :: rest ->
      ELet (a, x, ERef (a, RefCell, exp env e),
            block_intro (IdMap.add x true env) (rest, body))
  | funcs, rest ->
      let new_ids = map snd3 funcs in
      let env' = fold_left (fun acc f -> IdMap.add f true acc)  env new_ids in
      let f_exps = map (func_exp env') funcs in
        fold_right bind_func_ref f_exps 
          (fold_right set_func_ref f_exps
             (block_intro env' (rest, body)))

(* assumes [SeqExpr]s are nested to the right. *)
let rec flatten_seq (expr : expr) : expr list = match expr with
    SeqExpr (_, e1, e2) -> e1 :: flatten_seq e2
  | _ -> [ expr ]

let constructor_re = Str.regexp " *constructor.*"

let is_constructor_hint hint_txt =
  let r = Str.string_match constructor_re hint_txt 0 in
    r

let match_constr_body env expr = match expr with
    (* we drop the LabelledExpr, which will prevents us from using
        return within a constructor. *)
    HintExpr (p'', txt, FuncStmtExpr (p, f, args, LabelledExpr (p', l, body)))
      when is_constructor_hint txt ->
        let typ = match parse_annotation p'' txt with
          | AConstructor typ -> typ 
          | _ -> raise 
              (Not_well_formed (p'', "expected a constructor annotation")) in 
            let args = "this"::args in
            let locally_defined_vars = locals body in
            let visible_free_vars = 
              IdSet.diff (IdSetExt.from_list (IdMapExt.keys env))
                locally_defined_vars in
            let lambda_bound_vars = IdSetExt.from_list args in
            let locally_shadowed_args = 
              IdSet.inter lambda_bound_vars locally_defined_vars in
              if not (IdSet.is_empty locally_shadowed_args) then
                begin
                  IdSetExt.p_set FormatExt.text
                    locally_shadowed_args Format.str_formatter;
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
                                _, ThisExpr _, ConstExpr (_, S.CString fname)),
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
                  ELet (p, id, ERef (p, RefCell, EId (p, id)), exp) in
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
  | _ -> None


(** [match_func_decl expr] matches function statements and variables bound to
    function expressions. *)
let match_func_decl expr = match expr with
  | VarDeclExpr (p, x, e) ->
      if is_value e then Some (p, x, e) else None
  | HintExpr (p', txt, FuncStmtExpr (p, f, args, e)) ->
      if is_constructor_hint txt then
        None
      else
        Some (p, f, HintExpr (p', txt, FuncExpr (p, args, e)))
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
          _, VarExpr (_, cname), ConstExpr (_, S.CString "prototype")),
        ConstExpr (_, S.CString methodname)),
      methodexpr) -> Some (p, cname, methodname, methodexpr)
  | _ -> None


and bind_top_func_ref (x, t, e) rest_exp = 
  let p = Exp.pos e in
    DLet (p, x, ERef (p, RefCell, ESubsumption (p, t, EBot p)), rest_exp)

and set_top_func_ref (x, t, e) rest_exp =
  let p = Exp.pos e in
    DExp (ESetRef (p, EId (p, x), e), rest_exp)

let rec defs env lst = 
  begin match lst with
      [] -> DEnd
    | expr :: lst' ->
        begin match match_constr_body env expr with
          | Some c -> 
              DConstructor (c, defs (IdMap.add c.constr_name true env) lst')
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
                          DLet (p, x, ERef (p, RefCell, exp env expr),
                                defs env' lst')
                  end
                | func_binds, lst' ->
                    let mk acc (_, f, _) = IdMap.add f true acc in
                    let env' = fold_left mk env func_binds in
                    let f_exps = map (func_exp env') func_binds in
                      fold_right bind_top_func_ref f_exps 
                        (fold_right set_top_func_ref f_exps
                           (defs env' lst'))
              end
        end
  end

let from_exprjs env expr = 
  let exp = defs 
    (IdSet.fold (fun x env -> IdMap.add x true env) (Env.dom env) IdMap.empty)
    (flatten_seq expr) in
    exp

open Prelude
open Typedjs_syntax
open Exprjs_syntax
open Typedjs_env
open TypImpl
open Typedjs_tc_util

module H = Hashtbl
module S = JavaScript_syntax

module Desugar = Sb_desugar

let desugar_typ = Desugar.desugar_typ

exception Not_well_formed of pos * string

let parse_annotation (pos, end_p) str =
  let lexbuf = Lexing.from_string str in
    lexbuf.Lexing.lex_start_p <- pos;
    lexbuf.Lexing.lex_curr_p <- pos;
    try
      Typedjs_parser.typ_ann Typedjs_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "error lexing annotation at %s"
                       (string_of_position
                          (lexbuf.Lexing.lex_curr_p, lexbuf.Lexing.lex_curr_p)))
      |  Typedjs_parser.Error ->
           failwith (sprintf "error parsing annotation at %s"
                       (string_of_position
                          (lexbuf.Lexing.lex_curr_p, lexbuf.Lexing.lex_curr_p)))

(* TODO(arjun): fix!! *)
let bad_p = (Lexing.dummy_pos, Lexing.dummy_pos)

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
  | FuncExpr _ -> true
  |  _ -> false

let is_empty lst = match lst with
    [] -> true
  | _ -> false

type env = bool IdMap.t

let to_string  p e = EPrefixOp (p, "%ToString",  e) 
let to_object  p e = e (* EPrefixOp (p, "%ToObject",  e) *) (* HACK!! *)
let to_boolean p e = EPrefixOp (p, "%ToBoolean", e)

(** [object_and_function expr] is used on the expression in function
    position, [AppExpr (expr, _)]. It returns two expressions, [(this,
    func)], where [this] computes the implicit [this] argument and
    [func] is the function. The returned [func] may have a free
    variable, [%this].
*)
let rec object_and_function p (expr : expr) : expr * expr = match expr with
  | BracketExpr (p, e1, e2) -> (e1, BracketExpr (p, IdExpr (p, "%this"), e2))
  | ParenExpr (p, e) -> 
    let (this, func) = object_and_function p e in
    (this, ParenExpr (p, e))
  | expr -> (IdExpr (bad_p, "%global"), expr)

let forin_ix_typ = 
  TSource (TRegex (P.negate (P.parse Lexing.dummy_pos "__proto__")))

let rec exp (env : env) expr = match expr with
  | ConstExpr (a, c) -> EConst (a, c)
  | ArrayExpr (a, es) -> 
      ERef (a, RefCell, EArray (a, map (fun e -> exp env e) es))
  | ObjectExpr (a, ps) -> 
      if List.length ps != List.length (nub (map (fun (_, p, _) -> p) ps)) then
        raise (Not_well_formed (a, "repeated field names"));
    let ps = map (fun (_, x, e) -> x, exp env e) ps in
    let ps = if List.mem_assoc "__proto__" ps then ps
      else ("__proto__", exp env (VarExpr(a, "Object")))::ps in
    let ps = if List.mem_assoc "-*- code -*-" ps then ps
      else ("-*- code -*-", (* EAssertTyp(a, TUnion (TPrim "Undef",  *)
                            (*                       TArrow([TId "Ext"], Some (TId "Ext"), TId "Ext")), *)
                                       (EConst(a, JavaScript_syntax.CUndefined)))::ps in
    ERef (a, RefCell, EObject (a, ps))
  | ThisExpr a -> EDeref (a, EId (a, "this"))
  | VarExpr (a, x) -> begin try
      if IdMap.find x env then
        EDeref (a, EId (a, x))
      else
        EId (a, x)
    with Not_found -> EDeref (a, EId (a, x))
    end
  | IdExpr (a, x) -> EId (a, x)
  | BracketExpr (a, e1, e2) -> 
      EBracket (a, EDeref (a, to_object a (exp env e1)),
                to_string a (exp env e2))
  | NewExpr (p, constr, args) ->
    let constrPrototype = EBracket(p, EDeref(p, exp env constr), EConst(p, JavaScript_syntax.CString "prototype"))
    in ELet (p, "%newobj", ERef (p, RefCell, EObject (p, [("__proto__", constrPrototype)])), 
          ESeq 
            (p, 
             EApp (p, EBracket(p, EDeref(p, exp env constr), EConst(p, JavaScript_syntax.CString "-*- code -*-")),
                   (EId (p, "%newobj")) :: (map (exp env) args)),
             EId (p, "%newobj")))
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
  (* TODO: What is this hack below? *)
  | AppExpr (a, BracketExpr (a2, e1, 
                             ConstExpr (a3, JavaScript_syntax.CString "charAt")), [arg]) ->
      EInfixOp (a, "charAt", exp env e1, exp env arg)
  | AppExpr (p, obj_and_func, args) ->
    let (obj, func) = object_and_function p obj_and_func in
    ELet (p, "%this", exp env obj,
          EApp (p, EBracket(p, EDeref(p, exp env func), EConst(p, JavaScript_syntax.CString "-*- code -*-")),
                (EId (p, "%this")) :: (map (exp env) args)))
  | LetExpr (a, x, e1, e2) ->
      ELet (a, x, exp env e1, exp (IdMap.add x true env) e2)
  | TryCatchExpr (a, body, x, catch) ->
      ETryCatch (a, exp env body, x, exp env catch)
  | TryFinallyExpr (a, body, finally) -> 
      ETryFinally (a, exp env body, exp env finally)
  | ThrowExpr (a, e) -> EThrow (a, exp env e)
  | WhileExpr (a, break_lbls, e1, e2) ->
      let loop_typ = TArrow ([], None, TPrim "Undef") in
        ERec ([("%loop", loop_typ,
                EAssertTyp (a, loop_typ, 
                            EFunc (a, [], { func_loop = true;
                                            func_owned = IdSet.empty },
                                   List.fold_right (fun lbl body -> ELabel(a, lbl, body))
                                     break_lbls
                                     (EIf (a, exp env e1, 
                                           ESeq (a, exp env e2, 
                                                 EApp (a, EId (a, "%loop"), [])),
                                           EConst (a, S.CUndefined))))))],
              EApp (a, EId (a, "%loop"), []))
  | DoWhileExpr (a, break_lbls, body_e, test_e) ->
      let loop_typ = TArrow ([], None, TPrim "Undef") in
        ERec ([("%loop", loop_typ,
                EAssertTyp (a, loop_typ,
                EFunc (a, [], { func_loop = true;
                                func_owned = IdSet.empty },
                       List.fold_right (fun lbl body -> ELabel(a, lbl, body))
                         break_lbls
                         (ESeq (a, exp env body_e, 
                                EIf (a, exp env test_e, 
                                     EApp (a, EId (a, "%loop"), []),
                                     EConst (a, S.CUndefined)))))))],
              EApp (a, EId (a, "%loop"), []))

  | LabelledExpr (a, x, e) -> ELabel (a, x, exp env e)
  | BreakExpr (a, x, e) -> EBreak (a, x, exp env e)
  | SeqExpr 
      (p,
       IfExpr (_, IdExpr _, ConstExpr (_, S.CUndefined), 
               ConstExpr (_, S.CUndefined)),
       next_expr) -> exp env next_expr
  | SeqExpr (a, VarDeclExpr(_, x, BotExpr _), e) ->
    ELet (a, x, ERef(a, RefCell, EAssertTyp(a, TUninit (ref None), EBot a)), exp env e)
  | BotExpr a -> EBot a
  | SeqExpr (a, _, _) -> block_intro env (seq expr)
  | VarDeclExpr (a, x, e) -> 
      (* peculiar code: body or block ends with var *)
      ELet (a, x, exp env e, EConst (a, S.CUndefined))
  | FuncExpr _ -> match_func env expr
  | FuncStmtExpr (p, _, _, _) ->
      raise (Not_well_formed (p, "funcasdasdtion is missing a type annotation"))
  | ForInExpr (p, break_lbls, x, obj, body) ->
    let loop_typ = TArrow ([forin_ix_typ], None, TPrim "Undef") in
    ERec ([("%loop", loop_typ,
            EAssertTyp (p, loop_typ, 
            EFunc (p, [x], {func_loop = true;
                            func_owned = IdSet.empty },
       (* TODO: not fully-faithful--stopping condition missing *)
                   List.fold_right (fun lbl body -> ELabel(p, lbl, body))
                     break_lbls
                     (ESeq (p, exp env body, 
                            EApp (p, EId (p, "%loop"), 
                                  [ ECheat (p, forin_ix_typ, EId (p, x)) ]))))))],
          EApp (p, EId (p, "%loop"), 
                [ ECheat (p, forin_ix_typ, EId (p, x)) ]))
  | ParenExpr (a, e) -> EParen (a, exp env e)

and match_func env expr = match expr with
  | FuncExpr (a, args, LabelledExpr (a', "%return", body)) ->
      if List.length args != List.length (nub args) then
        raise (Not_well_formed (a, "each argument must have a distinct name"));
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
        ERef(a, RefCell, 
             EObject(a, [("-*- code -*-",
                          EFunc (a, args, { func_loop = false;
                                            func_owned = IdSet.empty }, 
                                 fold_left mutable_arg
                                   (ELabel (a', "%return", exp env' body))
                                   args));
                         ("prototype", EBot a (* EAssertTyp(a, TId "Ext", EConst(a, JavaScript_syntax.CUndefined)) *));
                         ("__proto__", EDeref(a, EId (a, "Object")))])) (* TODO: Make this Function *)
  | FuncExpr (a, _, _) ->
      failwith ("expected a LabelledExpr at " ^ string_of_position a)
  | _ -> failwith "hamg --- really should not fail anymore"

and exp_seq env e = match e with
    SeqExpr (a, e1, e2) -> ESeq (a, exp env e1, exp env e2)
  | _ -> exp env e

and func_exp env (_, x, expr) = 
  let e = exp env expr in
    (x, e)

and bind_func_ref (x, e) rest_exp = 
  let p = Exp.pos e in
  ELet (bad_p, x, ERef (bad_p, RefCell, EAssertTyp(p, TId "Ext", EBot p)), rest_exp)

and set_func_ref (x, e) rest_exp =
  let p = Exp.pos e in
    ESeq (p, ESetRef (p, EId (p, x), e), rest_exp)
  
and block_intro env (decls, body) = match take_while is_func_decl decls with
    [], [] -> exp_seq env body
  | [], (a, x, e) :: rest ->
      ELet (a, x, ERef (bad_p, RefCell, exp env e),
            block_intro (IdMap.add x true env) (rest, body))
  | funcs, rest ->
      let new_ids = map snd3 funcs in
      let env' = fold_left (fun acc f -> IdMap.add f true acc)  env new_ids in
      let f_exps = map (func_exp env') funcs in
        fold_right bind_func_ref f_exps 
          (fold_right set_func_ref f_exps
             (block_intro env' (rest, body)))

let from_exprjs env expr = 
  exp 
    (IdMap.add "arguments" false 
       (IdSet.fold (fun x env -> IdMap.add x true env) 
    (dom env) IdMap.empty))
    expr


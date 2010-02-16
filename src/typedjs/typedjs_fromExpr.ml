(** In this module, we transform ExprJS into Typed JavaScript. The major
    transformations are as follows:

    1. We incorporate annotations from comments (e.g. type annotations).

    2. We do away with scope objects.

    3. We remove [WhileExpr]s, transforming them into recursive functions.
*)

open Prelude
open Typedjs_syntax
open Exprjs_syntax
open Typedjs_types

let type_from_comment ((pos, end_p), str) =
  let lexbuf = Lexing.from_string str in
    lexbuf.Lexing.lex_start_p <- pos;
    lexbuf.Lexing.lex_curr_p <- pos;
    Typedjs_parser.typ_ann Typedjs_lexer.token lexbuf

let rec types_from_comments lst = match lst with
    [] -> PosMap.empty
  | (pos, str) :: rest when String.length str > 0 && str.[0] == ':' ->
      PosMap.add pos (type_from_comment (pos, str)) (types_from_comments rest)
  | _ :: rest -> types_from_comments rest

let type_dbs : annotation PosMap.t ref = ref PosMap.empty

let init_types lst = 
  type_dbs := types_from_comments lst

let type_between (start_p : Lexing.position) (end_p : Lexing.position) : typ =
  let found = ref None in
  let f (typ_start_p, typ_end_p) ann = 
    if typ_start_p.Lexing.pos_cnum > start_p.Lexing.pos_cnum &&
      typ_end_p.Lexing.pos_cnum < end_p.Lexing.pos_cnum then
        begin match ann, !found with
            ATyp typ, None -> found := Some ((typ_start_p, typ_end_p), typ)
          | _, Some _ ->
              failwith (sprintf "found multiple type annotations at %s"
                          (string_of_position (start_p, end_p)))

          | _, None -> failwith 
              (sprintf "expected a type annotation at %s; found something else"
                 (string_of_position (start_p, end_p)))

        end in
    PosMap.iter f !type_dbs;
    match !found with
        Some (pos, typ) -> 
          type_dbs := PosMap.remove pos !type_dbs;
          typ
      | None ->
          failwith (sprintf "expected a type annotation at %s"
                      (string_of_position (start_p, end_p)))


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

type env = IdSet.t

let rec exp (env : env) expr = match expr with
    StringExpr (a, s) -> EString (a, s)
  | RegexpExpr (a, re, g, i) -> ERegexp (a, re, g, i)
  | NumExpr (a, f) -> ENum (a, f)
  | IntExpr (a, n) -> EInt (a, n)
  | BoolExpr (a, b) -> EBool (a, b)
  | NullExpr a -> ENull a
  | ArrayExpr (a, es) -> EArray (a, map (exp env) es)
  | ObjectExpr (a, ps) -> 
      if List.length ps != List.length (nub (map fst2 ps)) then
        failwith (sprintf "duplicate field names in the object literal at %s"
                    (string_of_position a));
      EObject (a, map (second2 (exp env)) ps)
  | ThisExpr a -> EThis a
  | VarExpr (a, x) ->
      if IdSet.mem x env then EId (a, x)
      else failwith (sprintf "%s is unbound at %s" x (string_of_position a))
  | BracketExpr (a, e1, e2) -> EBracket (a, exp env e1, exp env e2)
  | NewExpr (a, c, args) -> ENew (a, exp env c, map (exp env) args)
  | PrefixExpr (a, op, e) -> EPrefixOp (a, op, exp env e)
  | InfixExpr (a, op, e1, e2) -> EInfixOp (a, op, exp env e1, exp env e2)
  | IfExpr (a, e1, e2, e3) -> EIf (a, exp env e1, exp env e2, exp env e3)
  | AssignExpr (a, lv, e) -> EAssign (a, lvalue env lv, exp env e)
  | AppExpr (a, f, args) -> EApp (a, exp env f, map (exp env) args)
  | FuncExpr (a, args, LabelledExpr (a', "%return", body)) ->
      if List.length args != List.length (nub args) then
        failwith (sprintf "each argument must have a distinct name at %s"
                    (string_of_position a));
      let typ = type_between (fst2 a) (fst2 a') in
        (* Within [body], a free variable, [x] cannot be referenced, if there
           is any local variable with the name [x]. *)
      let visible_free_vars = IdSet.diff env (locals body) in
      let env' = IdSet.union visible_free_vars (IdSetExt.from_list args) in
        (match typ with
             TArrow (_, _, r) ->
               EFunc (a, args, typ, ELabel (a', "%return", r, exp env' body))
           | _ ->
               failwith (sprintf "expected an arrow type on the function at %s"
                           (string_of_position a)))
  | FuncExpr (a, _, _) ->
      failwith (sprintf "TypedJS error at %s: expected a LabelledExpr"
                  (string_of_position a))
  | UndefinedExpr a -> EUndefined a
  | LetExpr (a, x, e1, e2) ->
      ELet (a, x, exp env e1, exp (IdSet.add x env) e2)
  | TryCatchExpr (a, body, x, catch) ->
      ETryCatch (a, exp env body, x, exp env catch)
  | TryFinallyExpr (a, body, finally) -> 
      ETryFinally (a, exp env body, exp env finally)
  | ThrowExpr (a, e) -> EThrow (a, exp env e)
  | WhileExpr (a, e1, e2) ->
      let loop_typ = TArrow (TTop, [], typ_undef) in
        ERec ([("%while", loop_typ,
                EFunc (a, [], loop_typ,
                       EIf (a, exp env e1, 
                            ESeq (a, exp env e2, 
                                  EApp (a, EId (a, "%loop"), [])),
                            EUndefined a)))],
              EApp (a, EId (a, "%loop"), []))
  | DoWhileExpr (a, e1, e2) ->
      let loop_typ = TArrow (TTop, [], typ_undef) in
        ERec ([("%while", loop_typ,
                EFunc (a, [], loop_typ,
                       ESeq (a, exp env e1, 
                             EIf (a, exp env e2, 
                                  EApp (a, EId (a, "%loop"), []),
                                    EUndefined a))))],
              EApp (a, EId (a, "%loop"), []))

  | LabelledExpr (a, x, e) -> 
      (** We assume that this [LabelledExpr] is from a [LabelledStmt]. 
          Therefore, the return type is [ty_undef]. *)
      ELabel (a, x, typ_undef, exp env e)
  | BreakExpr (a, x, e) -> EBreak (a, x, exp env e)
  | SeqExpr (a, _, _) -> block_intro env (seq expr)
  | VarDeclExpr (a, x, e) -> 
      (* peculiar code: body or block ends with var *)
      ELet (a, x, exp env e, EUndefined a)
  | FuncStmtExpr (a, f, args, LabelledExpr (a', "%return", body)) -> 
      (*  peculiar code: body or block ends with a function *)
      let typ = type_between (fst2 a) (fst2 a') in
      let visible_free_vars = IdSet.diff env (locals body) in
      let env' = IdSet.union visible_free_vars (IdSetExt.from_list args) in
        (match typ with
             TArrow (_, _, r) ->
               ELet (a, f,
                     EFunc (a, args, typ,
                            ELabel (a', "%return", r, exp env' body)),
                     EUndefined a)
           | _ ->
               failwith (sprintf "expected an arrow type on the function at %s"
                           (string_of_position a)))
  | FuncStmtExpr (a, _, _, _) ->
      failwith (sprintf "TypedJS error at %s: expected a LabelledExpr"
                  (string_of_position a))


and exp_seq env e = match e with
    SeqExpr (a, e1, e2) -> ESeq (a, exp env e1, exp env e2)
  | _ -> exp env e

and block_intro env (decls, body) = match take_while is_func_decl decls with
    [], [] -> exp_seq env body
  | [], (a, x, e) :: rest ->
      ELet (a, x, exp env e,
            block_intro (IdSet.add x env) (rest, body))
  | funcs, rest ->
      let new_ids = map snd3 funcs in
      let env' = IdSet.union env (IdSetExt.from_list new_ids) in
      let mk_bind (_, x, expr) = 
        let e = exp env' expr in
          (match e with
               EFunc (_, _, t, _) -> (x, t, e)
             | _ -> failwith ("TypedJS error: expected a FuncExpr")) in
        ERec (map mk_bind funcs,
              block_intro env' (rest, body))

and lvalue env lv = match lv with
    VarLValue (a, x) -> LVar (a, x)
  | PropLValue (a, e1, e2) -> LProp (a, exp env e1, exp env e2)

let from_exprjs expr = exp IdSet.empty expr

open Prelude
open Typedjs_syntax
open Exprjs_syntax
open Typedjs_types
open Typedjs_env

exception Not_well_formed of pos * string

let type_from_comment ((pos, end_p), str) =
  let lexbuf = Lexing.from_string str in
    lexbuf.Lexing.lex_start_p <- pos;
    lexbuf.Lexing.lex_curr_p <- pos;
    Typedjs_parser.typ_ann Typedjs_lexer.token lexbuf

let func_index = ref 0

let type_dbs : annotation PosMap.t ref = ref PosMap.empty

let inferred_array : annotation array ref = ref (Array.make 0 AMutable)

let rec init_types lst = match lst with
    [] -> ()
  | (pos, str) :: rest when String.length str > 0 && str.[0] == ':' ->
      begin match type_from_comment (pos, str) with
          AInferred xs -> 
            inferred_array := Array.of_list xs
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

let is_mutable_between start_p end_p : bool =
  match annotation_between start_p end_p with
      Some (pos, AMutable) -> 
        type_dbs := PosMap.remove pos !type_dbs;
        true
    | None -> false
    | Some _ ->
        raise (Not_well_formed ((start_p, end_p),
                                "unexpected annotation"))

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
      if List.length ps != List.length (nub (map (fun (_,p,_) -> p) ps)) then
        raise (Not_well_formed (a, "repeated field names"));
      let prop ((start_p, end_p), x, e) = 
        (x, is_mutable_between start_p end_p, exp env e) in
        EObject (a, map prop ps)
  | ThisExpr a -> EThis a
  | VarExpr (a, x) ->
      if IdSet.mem x env then EId (a, x)
      else (raise (Not_well_formed (a, x ^ " is not defined")))
  | BracketExpr (a, e1, e2) -> EBracket (a, exp env e1, exp env e2)
  | NewExpr (a, c, args) -> ENew (a, exp env c, map (exp env) args)
  | PrefixExpr (a, op, e) -> EPrefixOp (a, op, exp env e)
  | InfixExpr (a, op, e1, e2) -> EInfixOp (a, op, exp env e1, exp env e2)
  | IfExpr (a, e1, e2, e3) -> EIf (a, exp env e1, exp env e2, exp env e3)
  | AssignExpr (a, lv, e) -> EAssign (a, lvalue env lv, exp env e)
  | AppExpr (a, f, args) -> EApp (a, exp env f, map (exp env) args)
  | FuncExpr _ -> 
      (match match_func env expr with
           Some (_, e) -> e
         | None -> failwith "match_func returned None on a FuncExpr (1)")
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
  | FuncStmtExpr (a, f, args, body) ->
      begin match match_func (IdSet.add f env) (FuncExpr (a, args, body)) with
          Some (t, e) ->
            ERec ([ (f,  t, e) ], EUndefined a)
        | None -> failwith "match_func returned None on a FuncExpr (2)"
      end

and match_func env expr = match expr with
  | FuncExpr (a, args, LabelledExpr (a', "%return", body)) ->
      if List.length args != List.length (nub args) then
        raise (Not_well_formed (a, "each argument must have a distinct name"));
      let typ = type_between (fst2 a) (fst2 a') in
        (* Within [body], a free variable, [x] cannot be referenced, if there
           is any local variable with the name [x]. *)
      let locally_defined_vars = locals body in
      let visible_free_vars = IdSet.diff env locally_defined_vars in
      let lambda_bound_vars = IdSetExt.from_list args in
      let locally_shadowed_args = 
        IdSet.inter lambda_bound_vars locally_defined_vars in
      let env' = IdSet.union visible_free_vars lambda_bound_vars in
        if not (IdSet.is_empty locally_shadowed_args) then
          begin
            IdSetExt.pretty Format.str_formatter Format.pp_print_string 
              locally_shadowed_args;
            raise (Not_well_formed 
                     (a, "the following arguments are redefined as local \
                          variables: " ^ Format.flush_str_formatter ()))
          end;
        begin match typ with
            TArrow (_, _, r) ->
              incr func_index;
              Some (typ, EFunc (a, args, typ, 
                                ELabel (a', "%return", r, exp env' body)))
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
      ELet (a, x, exp env e,
            block_intro (IdSet.add x env) (rest, body))
  | funcs, rest ->
      let new_ids = map snd3 funcs in
      let env' = IdSet.union env (IdSetExt.from_list new_ids) in
      let mk_bind (_, x, expr) = 
        let e = exp env' expr in
          (match e with
               EFunc (_, _, t, _) -> (x, t, e)
             | _ -> failwith "expected a FuncExpr") in
        ERec (map mk_bind funcs,
              block_intro env' (rest, body))

and lvalue env lv = match lv with
    VarLValue (a, x) -> LVar (a, x)
  | PropLValue (a, e1, e2) -> LProp (a, exp env e1, exp env e2)

(* assumes [SeqExpr]s are nested to the right. *)
let rec flatten_seq (expr : expr) : expr list = match expr with
    SeqExpr (_, e1, e2) -> e1 :: flatten_seq e2
  | _ -> [ expr ]



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


let rec defs env lst = match match_while match_func_decl lst with
    [], [] -> DEnd
  | [], expr :: lst' -> begin match match_decl expr with
        None -> DExp (exp env expr, defs env lst')
      | Some (p, x, expr) -> DLet (p, x, exp env expr, defs env lst)
    end
  | func_binds, lst' ->
      let env' = IdSet.union (IdSetExt.from_list (map snd3 func_binds)) env in
      let mk_bind (_, x, expr) = match match_func env' expr with
          Some (t, e) -> (x, t, e)
        | None -> failwith "match_func returned none on a FuncExpr (defs)" in
        DRec (map mk_bind func_binds, defs env' lst')


let from_exprjs expr = 
  defs (IdSetExt.from_list (IdMapExt.keys init_env)) (flatten_seq expr)

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

open Prelude
open JavaScript_syntax

type contract =
  | CPred of string * expr
  | CArrow of contract list * contract

module type Contracts = sig

  val contract_lib : expr

  val contract_parser : pos -> string -> contract option

end

module Make ( C : Contracts) = struct

  open C

  let rec cc p (ctc : contract) : expr = match ctc with
    | CPred (name, expr) -> 
        (* predicates are already wrapped *)
        expr 
    | CArrow (args, result) ->
        CallExpr
          (p, CallExpr (p, DotExpr (p, contract_lib, "varArityFunc"),
                        [ ConstExpr (p, CString "function") ]),
           [ ArrayExpr (p, map (cc p) args); 
             ArrayExpr (p, []); (* zero var-arity arguments *)
             cc p result ])

  let rec ic_expr (expr : expr) = match expr with
    | ConstExpr _ -> expr
    | ArrayExpr (p, es) -> ArrayExpr (p, map ic_expr es)
    | ObjectExpr (p, props) -> ObjectExpr (p, map (third3 ic_expr) props)
    | ThisExpr _ -> expr
    | VarExpr _ -> expr
    | DotExpr (p, e, x) -> DotExpr (p, ic_expr e, x)
    | BracketExpr (p, e1, e2) -> 
        (* wrap array accesses: *)
        let be = BracketExpr (p, ic_expr e1, ic_expr e2) in
          begin match e2 with
            | ConstExpr (p2, CInt i) -> 
                CallExpr (p, DotExpr (p, contract_lib, "guard"),
                          [ DotExpr (p, contract_lib, "NotUndefined"); 
                            be; 
                            ConstExpr (p, CString "callee");
                            ConstExpr (p, CString "caller");
                            ConstExpr (p, CString (string_of_position p)) ])
            | _ -> be
          end
    | NewExpr (p, e, es) -> NewExpr (p, ic_expr e, map ic_expr es)
    | PrefixExpr (p, op, e) -> PrefixExpr (p, op, ic_expr e)
    | UnaryAssignExpr (p, op, lv) -> UnaryAssignExpr (p, op, ic_lvalue lv)
    | InfixExpr (p, op, e1, e2) -> InfixExpr (p, op, ic_expr e1, ic_expr e2)
    | IfExpr (p, e1, e2, e3) -> IfExpr (p, ic_expr e1, ic_expr e2, ic_expr e3)
    | AssignExpr (p, op, lv, e) -> AssignExpr (p, op, ic_lvalue lv, ic_expr e)
    | ParenExpr (p, e) -> ParenExpr (p, ic_expr e)
    | ListExpr (p, e1, e2) -> ListExpr (p, ic_expr e1, ic_expr e2)
    | CallExpr (p, e, es) -> CallExpr (p, ic_expr e, map ic_expr es)
    | FuncExpr (p, args, s) -> FuncExpr (p, args, ic_stmt s)
    | NamedFuncExpr (p, f, args, s) -> NamedFuncExpr (p, f, args, ic_stmt s)
    | HintExpr (p, text, e) -> begin match contract_parser p text with
        | None -> ic_expr e
        | Some contract -> 
            CallExpr (p, DotExpr (p, contract_lib, "guard"),
                      [ cc p contract; ic_expr e;
                        ConstExpr (p, CString "callee");
                        ConstExpr (p, CString "caller");
                        ConstExpr (p, CString (string_of_position p)) ])
      end

  and ic_stmt (stmt : stmt) : stmt = match stmt with
    | BlockStmt (p, ss) -> BlockStmt (p, map ic_stmt ss)
    | EmptyStmt _ -> stmt
    | ExprStmt e -> ExprStmt (ic_expr e)
    | IfStmt (p, e1, s2, s3) -> IfStmt (p, ic_expr e1, ic_stmt s2, ic_stmt s3)
    | IfSingleStmt (p, e1, s2) -> IfSingleStmt (p, ic_expr e1, ic_stmt s2)
    | SwitchStmt (p, e, cases) -> SwitchStmt (p, ic_expr e, map ic_case cases)
    | WhileStmt (p, e, s) -> WhileStmt (p, ic_expr e, ic_stmt s)
    | DoWhileStmt (p, s, e) -> DoWhileStmt (p, ic_stmt s, ic_expr e)
    | BreakStmt _ -> stmt
    | BreakToStmt _ -> stmt
    | ContinueStmt _ -> stmt
    | ContinueToStmt _ -> stmt
    | LabelledStmt (p, x, s) -> LabelledStmt (p, x, ic_stmt s)
    | ForInStmt (p, fii, e, s) -> ForInStmt (p, fii, ic_expr e, ic_stmt s)
    | ForStmt (p, i1, e2, e3, s4) -> 
        ForStmt (p, ic_forInit i1, ic_expr e2, ic_expr e3, ic_stmt s4)
    | TryStmt (p, s1, cs, s2) ->
        TryStmt (p, ic_stmt s1, map ic_catch cs, ic_stmt s2)
    | ThrowStmt (p, e) -> ThrowStmt (p, ic_expr e)
    | ReturnStmt (p, e) -> ReturnStmt (p, ic_expr e)
    | WithStmt (e, s) -> WithStmt (ic_expr e, ic_stmt s)
    | VarDeclStmt (p, ds) -> VarDeclStmt (p, map ic_varDecl ds)
    | FuncStmt (p, f, args, s) -> FuncStmt (p, f, args, ic_stmt s)
    | HintStmt (p, text, FuncStmt (p', f, args, s)) -> 
        (* Warning: function statement transformed into an expression. *)
        begin match contract_parser p text with
          | None -> FuncStmt (p', f, args, ic_stmt s)
          | Some contract ->
              let e = 
                CallExpr (p, DotExpr (p, contract_lib, "guard"),
                          [ cc p contract; 
                            ic_expr (FuncExpr (p', args, ic_stmt s));
                            ConstExpr (p', CString "callee");
                            ConstExpr (p', CString "caller");
                            ConstExpr (p', CString (string_of_position p)) ]) in
                VarDeclStmt (p, [ VarDecl (p, f, e) ])
        end
    | HintStmt (_, _, s) -> ic_stmt s

  and ic_varDecl (decl : varDecl) : varDecl = match decl with
    | VarDeclNoInit _ -> decl
    | VarDecl (p, x, e) -> VarDecl (p, x, ic_expr e)

  and ic_forInit (forInit : forInit) : forInit = match forInit with
    | NoForInit -> NoForInit
    | VarForInit ds -> VarForInit (map ic_varDecl ds)
    | ExprForInit e -> ExprForInit (ic_expr e)

  and ic_catch (CatchClause (p, x, s)) = CatchClause (p, x, ic_stmt s)

  and ic_case (clause : caseClause) : caseClause = match clause with
    | CaseClause (p, e, s) -> CaseClause (p, ic_expr e, ic_stmt s)
    | CaseDefault (p, s) -> CaseDefault (p, ic_stmt s)

  and ic_lvalue (lv : lvalue) : lvalue = match lv with
    | VarLValue _ -> lv
    | DotLValue (p, e, x) -> DotLValue (p, ic_expr e, x)
    | BracketLValue (p, e1, e2) -> BracketLValue (p, ic_expr e1, ic_expr e2)

  let insert_contracts (Prog (p, ss)) = Prog (p, map ic_stmt ss)

end

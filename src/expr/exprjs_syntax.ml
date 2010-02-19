open Prelude

type expr
  = StringExpr of pos * string
  | RegexpExpr of pos * string * bool * bool
  | NumExpr of pos * float
  | IntExpr of pos * int
  | BoolExpr of pos * bool
  | NullExpr of pos
  | ArrayExpr of pos * expr list
  | ObjectExpr of pos * (pos * string * expr) list
  | ThisExpr of pos
  | VarExpr of pos * id
  | BracketExpr of pos * expr * expr
  | NewExpr of pos * expr * expr list
  | PrefixExpr of pos * JavaScript_syntax.prefixOp * expr
  | InfixExpr of pos * JavaScript_syntax.infixOp * expr * expr
  | IfExpr of pos * expr * expr * expr
  | AssignExpr of pos * lvalue * expr
  | AppExpr of pos * expr * expr list
  | FuncExpr of pos * id list * expr
  | UndefinedExpr of pos
  | LetExpr of pos * id * expr * expr
  | SeqExpr of pos * expr * expr
  | WhileExpr of pos * expr * expr
  | DoWhileExpr of pos * expr * expr
  | LabelledExpr of pos * id * expr
  | BreakExpr of pos * id * expr
  | ForInExpr of pos * id * expr * expr
  | VarDeclExpr of pos * id * expr
  | TryCatchExpr of pos * expr * id * expr
  | TryFinallyExpr of pos * expr * expr
  | ThrowExpr of pos * expr
  | FuncStmtExpr of pos * id * id list * expr

and lvalue =
    VarLValue of pos * id
  | PropLValue of pos * expr * expr

(******************************************************************************)

open JavaScript_stxutil
module S = JavaScript_syntax

let dum = (Lexing.dummy_pos, Lexing.dummy_pos)

let infix_of_assignOp op = match op with
    S.OpAssignAdd -> S.OpAdd
  | S.OpAssignSub -> S.OpSub
  | S.OpAssignMul -> S.OpMul
  | S.OpAssignDiv -> S.OpDiv
  | S.OpAssignMod -> S.OpMod
  | S.OpAssignLShift -> S.OpLShift
  | S.OpAssignSpRShift -> S.OpSpRShift
  | S.OpAssignZfRShift -> S.OpZfRShift
  | S.OpAssignBAnd -> S.OpBAnd
  | S.OpAssignBXor -> S.OpBXor
  | S.OpAssignBOr -> S.OpBOr
  | S.OpAssign -> failwith "infix_of_assignOp applied to OpAssign"

let seq a e1 e2 = SeqExpr (a, e1, e2)

let rec expr (e : S.expr) = match e with
    S.StringExpr (a,s) -> StringExpr (a,s)
  | S.RegexpExpr (a,re,g,i) -> RegexpExpr (a,re,g,i)
  | S.NumExpr (a,f) -> NumExpr (a,f)
  | S.IntExpr (a,n) -> IntExpr (a,n)
  | S.BoolExpr (a,b) -> BoolExpr (a,b)
  | S.NullExpr a -> NullExpr a
  | S.ArrayExpr (a,es) -> ArrayExpr (a,map expr es)
  | S.ObjectExpr (a,ps) -> ObjectExpr (a,map prop ps)
  | S.ThisExpr a -> ThisExpr a
  | S.VarExpr (a,x) -> VarExpr (a,x)
  | S.DotExpr (a,e,x) -> BracketExpr (a, expr e, StringExpr (a, x))
  | S.BracketExpr (a,e1,e2) -> BracketExpr (a,expr e1,expr e2)
  | S.NewExpr (a,e,es) -> NewExpr (a,expr e,map expr es)
  | S.PrefixExpr (a,op,e) -> PrefixExpr (a,op,expr e)
  | S.UnaryAssignExpr (a, op, lv) ->
      let func (lv, e) = 
        (match op with
             S.PrefixInc ->
               seq a
                 (AssignExpr 
                    (a, lv, InfixExpr (a, S.OpAdd, e, IntExpr (a, 1))))
                 e
           | S.PrefixDec ->
               seq
                 a
                 (AssignExpr 
                    (a, lv, InfixExpr (a, S.OpSub, e, IntExpr (a, 1))))
                  e
           | S.PostfixInc ->
               LetExpr
                 (a, "%postfixinc", e,
                  seq a
                    (AssignExpr 
                       (a, lv, InfixExpr 
                          (* TODO: use numeric addition with casts. *)
                          (a, S.OpAdd, VarExpr (a, "%postfixinc"), 
                           IntExpr (a, 1))))
                    (VarExpr (dum, "%postfixinc")))
           | S.PostfixDec ->
               LetExpr
                 (a, "%postfixdec", e,
                  seq a
                    (AssignExpr 
                       (a, lv, InfixExpr 
                          (* TODO use numeric subtraction with casts *)
                          (a, S.OpSub, VarExpr (a, "%prefixinc"), 
                           IntExpr (a, 1))))
                       (VarExpr (a, "%prefixinc"))))
         in eval_lvalue lv func
  | S.InfixExpr (a,op,e1,e2) -> InfixExpr (a,op,expr e1,expr e2)
  | S.IfExpr (a,e1,e2,e3) -> IfExpr (a,expr e1,expr e2,expr e3)
  | S.AssignExpr (a,S.OpAssign,lv,e) -> AssignExpr (a,lvalue lv,expr e)
  | S.AssignExpr (a,op,lv,e) -> 
      let body_fn (lv,lv_e) = 
        AssignExpr (a,lv,InfixExpr (a,infix_of_assignOp op,lv_e,expr e))
      in eval_lvalue lv body_fn
  | S.ParenExpr (_,e) -> expr e
  | S.ListExpr (a,e1,e2) -> seq a (expr e1) (expr e2)
  | S.CallExpr (a,func,args) -> AppExpr (a,expr func,map expr args)
  | S.FuncExpr (a, args, body) ->
      FuncExpr (a, args,
                LabelledExpr (stmt_annotation body, "%return", stmt body))
  | S.UndefinedExpr a -> UndefinedExpr a
  | S.NamedFuncExpr (a, name, args, body) ->
      (* INFO: This translation is absurd and makes typing impossible.
         Introduce FIX and eliminate loops in the process. Note that the
         parser does not produce NamedFuncExprs, so for the moment, this is
         inconsequential. *)
      let anonymous_func = 
        FuncExpr (a,args,LabelledExpr (a,"%return",stmt body)) in
      LetExpr (a,name,UndefinedExpr a,
               seq a
                 (AssignExpr (a,
                              VarLValue (a,name),anonymous_func))
                 (VarExpr (a,name)))
                        
and lvalue (lv : S.lvalue) = match lv with
    S.VarLValue (a,x) -> VarLValue (a,x)
  | S.DotLValue (a,e,x) -> PropLValue (a,expr e,StringExpr (a,x))
  | S.BracketLValue (a,e1,e2) -> PropLValue (a,expr e1,expr e2)

and stmt (s : S.stmt) = match s with 
    S.BlockStmt (a,[]) -> UndefinedExpr a
  | S.BlockStmt (a,s1::ss) -> seq a (stmt s1) (stmt (S.BlockStmt (a, ss)))
  | S.EmptyStmt a -> UndefinedExpr a
  | S.IfStmt (a,e,s1,s2) -> IfExpr (a,expr e,stmt s1,stmt s2)
  | S.IfSingleStmt (a,e,s) -> IfExpr (a,expr e,stmt s,UndefinedExpr a)
  | S.SwitchStmt (p,e,clauses) ->
      LetExpr (p,"%v",expr e,
               LetExpr (p,"%t",BoolExpr (p,false),caseClauses p clauses))
  | S.LabelledStmt (p1, lbl ,S.WhileStmt (p2, test, body)) -> LabelledExpr 
        (p1, "%break", LabelledExpr
           (p1,lbl,WhileExpr
              (p2,expr test,LabelledExpr 
                 (p2,"%continue",LabelledExpr
                    (p1,"%continue-"^lbl,stmt body)))))
                             
                                             
  | S.WhileStmt (p,test,body) -> LabelledExpr
        (p,"%break",WhileExpr 
           (p,expr test,LabelledExpr 
              (p,"%continue",stmt body)))
  | S.LabelledStmt (p1, lbl ,S.DoWhileStmt (p2, body, test)) -> LabelledExpr 
        (p1, "%break", LabelledExpr
           (p1,lbl,DoWhileExpr
              (p2, LabelledExpr 
                 (p1,"%continue",LabelledExpr
                    (p2,"%continue-"^lbl,stmt body)),
              expr test)))
  | S.DoWhileStmt (p, body, test) -> LabelledExpr
      (p, "%break", WhileExpr 
           (p, LabelledExpr (p, "%continue", stmt body),
            expr test))
  | S.BreakStmt a -> BreakExpr (a,"%break",UndefinedExpr a)
  | S.BreakToStmt (a,lbl) -> BreakExpr (a,lbl,UndefinedExpr a)
  | S.ContinueStmt a -> BreakExpr (a,"%continue",UndefinedExpr a)
  | S.ContinueToStmt (a,lbl) -> BreakExpr (a,"%continue-"^lbl,UndefinedExpr a)
  | S.FuncStmt (a, f, args, s) -> 
      FuncStmtExpr 
        (a, f, args, LabelledExpr 
           (stmt_annotation s, "%return", stmt s))
  | S.ExprStmt e -> expr e
  | S.ThrowStmt (a, e) -> ThrowExpr (a, expr e)
  | S.ReturnStmt (a, e) -> BreakExpr (a, "%return", expr e)
  | S.WithStmt _ -> failwith "we do not account for with statements"
  | S.TryStmt (a, body, catches, finally) ->
      let f body (S.CatchClause (a, x, s)) = TryCatchExpr (a, body, x, stmt s)
      in TryFinallyExpr (a, fold_left f (stmt body) catches, stmt finally)
  | S.ForStmt (a, init, incr, stop, body) ->
      seq a
        (forInit a init)
        (LabelledExpr 
           (a, "%break",
            WhileExpr 
              (a, expr stop, 
               seq a
                 (LabelledExpr (a, "%continue", stmt body))
                 (expr incr))))
  | S.ForInStmt (p, init, e, body) ->
      let (x, init_e) = forInInit init in
        SeqExpr 
          (p, init_e,
           LabelledExpr
             (p, "%break",
              ForInExpr 
                (p, x, expr e,
                 LabelledExpr
                   (p, "%continue", stmt body))))
  | S.VarDeclStmt (a, decls) -> varDeclList a decls
  | S.LabelledStmt (p, lbl, s) ->
      LabelledExpr (p, lbl, stmt s)

and forInit p (fi : S.forInit) = match fi with
    S.NoForInit -> UndefinedExpr p
  | S.ExprForInit e -> expr e
  | S.VarForInit decls -> varDeclList p decls

and forInInit fii = match fii with
    S.VarForInInit (p, x) -> (x, VarDeclExpr (p, x, UndefinedExpr p))
  | S.NoVarForInInit (p, x) -> (x, UndefinedExpr p)

and varDeclList p decls = match decls with
    [] -> UndefinedExpr p
  | [d] -> varDecl p d
  | d :: ds -> seq p (varDecl p d) (varDeclList p ds)

and varDecl p (decl : S.varDecl) = match decl with
    S.VarDeclNoInit (a, x) -> VarDeclExpr (a, x, UndefinedExpr p)
  | S.VarDecl (a, x, e) -> VarDeclExpr (a, x, expr e)

and caseClauses p (clauses : S.caseClause list) = match clauses with
    [] -> UndefinedExpr p
  | (S.CaseDefault (a,s)::clauses) -> seq a (stmt s) (caseClauses p clauses)
  | (S.CaseClause (a,e,s)::clauses) ->
      LetExpr (a,"%t",
               IfExpr (a,VarExpr (a,"%t"), 
                       (BoolExpr (a,true)),
                       (expr e)),
               SeqExpr (a,
                        IfExpr (a,VarExpr (a,"%t"),
                                stmt s,
                                UndefinedExpr a),
                        caseClauses p clauses))

and prop pr =  match pr with
    (p, S.PropId x,e) -> (p, x, expr e)
  | (p, S.PropString s,e) -> (p, s, expr e)
  | (p, S.PropNum n,e) -> (p, string_of_int n, expr e)


(** Generates an expression that evaluates and binds lv, then produces the
    the value of body_fn.  body_fn is applied with references to lv as an
    lvalue and lv as an expression. *)
and eval_lvalue (lv :  S.lvalue) (body_fn : lvalue * expr -> expr) =
  match lv with
    S.VarLValue (a,x) -> body_fn (VarLValue (a,x),VarExpr (a,x))
  | S.DotLValue (a,e,x) -> 
      LetExpr (a,"%lhs",expr e,
        body_fn (PropLValue (a,VarExpr (a,"%lhs"),StringExpr (a,x)),
                 BracketExpr (a,VarExpr (a,"%lhs"),StringExpr (a,x))))
  | S.BracketLValue (a,e1,e2) -> 
      LetExpr (a,"%lhs",expr e1,
      LetExpr (a,"%field",expr e2,
      body_fn (PropLValue (a,VarExpr (a,"%lhs"),VarExpr (a,"%field")),
               BracketExpr (a,VarExpr (a,"%lhs"),VarExpr (a,"%field")))))

let from_javascript stmts = 
  let f s e = seq dum (stmt s) e
  in fold_right f stmts (UndefinedExpr dum)

let from_javascript_expr = expr

(******************************************************************************)

let rec locals expr = match expr with
   StringExpr _ -> IdSet.empty
  | RegexpExpr _ -> IdSet.empty
  | NumExpr _ -> IdSet.empty
  | IntExpr _ -> IdSet.empty
  | BoolExpr _ -> IdSet.empty
  | NullExpr _ -> IdSet.empty
  | ArrayExpr (_, es) -> IdSetExt.unions (map locals es)
  | ObjectExpr (_, ps) -> IdSetExt.unions (map (fun (_, _, e) -> locals e) ps)
  | ThisExpr _ -> IdSet.empty
  | VarExpr _ -> IdSet.empty
  | BracketExpr (_, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | NewExpr (_, c, args) -> IdSetExt.unions (map locals (c :: args))
  | PrefixExpr (_, _, e) -> locals e
  | InfixExpr (_, _, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | IfExpr (_, e1, e2, e3) -> IdSetExt.unions (map locals [e1; e2; e3])
  | AssignExpr (_, l, e) -> IdSet.union (lv_locals l) (locals e)
  | AppExpr (_, f, args) -> IdSetExt.unions (map locals (f :: args))
  | FuncExpr _ -> IdSet.empty
  | UndefinedExpr _ -> IdSet.empty
  | LetExpr (_, _, e1, e2) -> 
      (* We are computing properties of the local scope object, not identifers
         introduced by the expression transformation. *)
      IdSet.union (locals e1) (locals e2)
  | SeqExpr (_, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | WhileExpr (_, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | DoWhileExpr (_, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | LabelledExpr (_, _, e) -> locals e
  | BreakExpr (_, _, e) -> locals e
  | ForInExpr (_, _, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | VarDeclExpr (_, x, e) -> IdSet.add x (locals e)
  | TryCatchExpr (_, e1, _, e2) ->
      (* TODO: figure out how to handle catch-bound identifiers *)
      IdSet.union (locals e1) (locals e2)
  | TryFinallyExpr (_, e1, e2) -> IdSet.union (locals e1) (locals e2)
  | ThrowExpr (_, e) -> locals e
  | FuncStmtExpr (_, f, _, _) -> IdSet.singleton f

and lv_locals lvalue = match lvalue with
    VarLValue _ -> IdSet.empty
  | PropLValue (_, e1, e2) -> IdSet.union (locals e1) (locals e2)

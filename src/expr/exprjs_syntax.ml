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
      (** Object properties are transformed into string literals *)
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
      (** We need let-expressions to simplify statements. *)
  | SeqExpr of pos * expr * expr
  | WhileExpr of pos * expr * expr
  | DoWhileExpr of pos * expr * expr
  | LabelledExpr of pos * id * expr
  | BreakExpr of pos * id * expr
  | ForInExpr of pos * id * expr * expr
  | VarDeclExpr of pos * id * expr
      (** We do not transform VarDeclStmts to let-bindings at this stage *)
  | TryCatchExpr of pos * expr * id * expr
  | TryFinallyExpr of pos * expr * expr
  | ThrowExpr of pos * expr
  | FuncStmtExpr of pos * id * id list * expr
      (** We leave function statements in place, so that they can be lifted
          for JavaScript to turned into letrecs for Typed JavaScript. *)


and lvalue =
    VarLValue of pos * id
  | PropLValue of pos * expr * expr

(******************************************************************************)

let rec aborts (expr : expr) : bool = match expr with
   StringExpr _ -> false
  | RegexpExpr _ -> false
  | NumExpr _ -> false
  | IntExpr _ -> false
  | BoolExpr _ -> false
  | NullExpr _ -> false
  | ArrayExpr (_, es) -> List.exists aborts es
  | ObjectExpr (_, ps) -> List.exists (fun (_, _, e) -> aborts e) ps
  | ThisExpr _ -> false
  | VarExpr _ -> false
  | BracketExpr (_, e1, e2) -> aborts e1 || aborts e2
  | NewExpr (_, c, args) -> List.exists aborts (c :: args)
      (* JavaScript's operators never signal exceptions--right? *)
  | PrefixExpr (_, _, e) -> aborts e
  | InfixExpr (_, _, e1, e2) -> aborts e1 || aborts e2
  | IfExpr (_, e1, e2, e3) -> aborts e1 || (aborts e2 && aborts e3)
  | AssignExpr (_, VarLValue _, e) -> aborts e
  | AssignExpr (_, PropLValue (_, e1, e2), e3) ->
      aborts e1 || aborts e2 || aborts e3
  | AppExpr (_, f, args) -> List.exists aborts (f :: args)
  | FuncExpr _  -> false
  | UndefinedExpr _ -> false
  | LetExpr (_, _, e1, e2) -> aborts e1 || aborts e2
  | SeqExpr (_, e1, e2) -> aborts e1 || aborts e2
  | WhileExpr (_, e1, e2) -> aborts e1 || aborts e2
  | DoWhileExpr (_, e1, e2) -> aborts e1 || aborts e2
  | LabelledExpr (_, _, e) -> aborts e
  | BreakExpr _ -> true
  | ForInExpr (_, _, e1, e2) -> aborts e1 || aborts e2
  (* We do not transform VarDeclStmts to let-bindings, yet *)
  | VarDeclExpr (_, _, e) -> aborts e 
  | TryCatchExpr (_, e1, _, e2) -> aborts e1 || aborts e2
  | TryFinallyExpr (_, _, e2) -> aborts e2
  | ThrowExpr (_, e) -> aborts e
  | FuncStmtExpr _ -> false

(******************************************************************************)

open JavaScript_stxutil
module S = JavaScript_syntax
module L = List

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

(** [seq a e1 e2] removes dead code from [SeqExpr] (code after a break) and
    rearranges sequences so that nested [SeqExpr]s are on the right. However,
    it breaks the syntactic compositionality that \JS touts. *)
let rec seq a e1 e2 = 
  if aborts e1 then e1
  else (match e1 with
          | SeqExpr (a', e11, e12) -> SeqExpr (a, e11, seq a' e12 e2)
          | _ -> SeqExpr (a, e1, e2))

let rec expr (e : S.expr) = match e with
    S.StringExpr (a,s) -> StringExpr (a,s)
  | S.RegexpExpr (a,re,g,i) -> RegexpExpr (a,re,g,i)
  | S.NumExpr (a,f) -> NumExpr (a,f)
  | S.IntExpr (a,n) -> IntExpr (a,n)
  | S.BoolExpr (a,b) -> BoolExpr (a,b)
  | S.NullExpr a -> NullExpr a
  | S.ArrayExpr (a,es) -> ArrayExpr (a,L.map expr es)
  | S.ObjectExpr (a,ps) -> ObjectExpr (a,L.map prop ps)
  | S.ThisExpr a -> ThisExpr a
  | S.VarExpr (a,x) -> VarExpr (a,x)
  | S.DotExpr (a,e,x) -> BracketExpr (a, expr e, StringExpr (a, x))
  | S.BracketExpr (a,e1,e2) -> BracketExpr (a,expr e1,expr e2)
  | S.NewExpr (a,e,es) -> NewExpr (a,expr e,L.map expr es)
  | S.PrefixExpr (a,op,e) -> PrefixExpr (a,op,expr e)
  | S.UnaryAssignExpr (a, op, lv) ->
      let func (lv, e) = 
        (match op with
             S.PrefixInc ->
               seq a
                 (AssignExpr 
                    (a, lv, InfixExpr (dum, S.OpAdd, e, IntExpr (dum, 1))))
                 e
           | S.PrefixDec ->
               seq
                 a
                 (AssignExpr 
                    (a, lv, InfixExpr (dum, S.OpSub, e, IntExpr (dum, 1))))
                  e
           | S.PostfixInc ->
               LetExpr
                 (dum, "%postfixinc", e,
                  seq a
                    (AssignExpr 
                       (a, lv, InfixExpr 
                          (* TODO: use numeric addition with casts. *)
                          (dum, S.OpAdd, VarExpr (dum, "%postfixinc"), 
                           IntExpr (dum, 1))))
                    (VarExpr (dum, "%postfixinc")))
           | S.PostfixDec ->
               LetExpr
                 (dum, "%postfixdec", e,
                  seq a
                    (AssignExpr 
                       (a, lv, InfixExpr 
                          (* TODO use numeric subtraction with casts *)
                          (dum, S.OpSub, VarExpr (dum, "%prefixinc"), 
                           IntExpr (dum, 1))))
                       (VarExpr (dum, "%prefixinc"))))
         in eval_lvalue lv func
  | S.InfixExpr (a,op,e1,e2) -> InfixExpr (a,op,expr e1,expr e2)
  | S.IfExpr (a,e1,e2,e3) -> IfExpr (a,expr e1,expr e2,expr e3)
  | S.AssignExpr (a,S.OpAssign,lv,e) -> AssignExpr (a,lvalue lv,expr e)
  | S.AssignExpr (a,op,lv,e) -> 
      let body_fn (lv,lv_e) = 
        AssignExpr (a,lv,InfixExpr (dum,infix_of_assignOp op,lv_e,expr e))
      in eval_lvalue lv body_fn
  | S.ParenExpr (_,e) -> expr e
  | S.ListExpr (a,e1,e2) -> seq a (expr e1) (expr e2)
  | S.CallExpr (a,func,args) -> AppExpr (a,expr func,L.map expr args)
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
      LetExpr (dum,name,UndefinedExpr dum,
               seq dum
                 (AssignExpr (dum,
                              VarLValue (dum,name),anonymous_func))
                 (VarExpr (dum,name)))
                        
and lvalue (lv : S.lvalue) = match lv with
    S.VarLValue (a,x) -> VarLValue (a,x)
  | S.DotLValue (a,e,x) -> PropLValue (a,expr e,StringExpr (dum,x))
  | S.BracketLValue (a,e1,e2) -> PropLValue (a,expr e1,expr e2)

and stmt (s : S.stmt) = match s with 
    S.BlockStmt (a,[]) -> UndefinedExpr a
  | S.BlockStmt (a,s1::ss) -> seq a (stmt s1) (stmt (S.BlockStmt (dum, ss)))
  | S.EmptyStmt a -> UndefinedExpr a
  | S.IfStmt (a,e,s1,s2) -> IfExpr (a,expr e,stmt s1,stmt s2)
  | S.IfSingleStmt (a,e,s) -> IfExpr (a,expr e,stmt s,UndefinedExpr dum)
  | S.SwitchStmt (p,e,clauses) ->
      LetExpr (p,"%v",expr e,
               LetExpr (dum,"%t",BoolExpr (dum,false),caseClauses clauses))
  | S.LabelledStmt (p1, lbl ,S.WhileStmt (p2, test, body)) -> LabelledExpr 
        (dum, "%break", LabelledExpr
           (p1,lbl,WhileExpr
              (p2,expr test,LabelledExpr 
                 (dum,"%continue",LabelledExpr
                    (dum,"%continue-"^lbl,stmt body)))))
                             
                                             
  | S.WhileStmt (p,test,body) -> LabelledExpr
        (dum,"%break",WhileExpr 
           (p,expr test,LabelledExpr 
              (dum,"%continue",stmt body)))
  | S.LabelledStmt (p1, lbl ,S.DoWhileStmt (p2, body, test)) -> LabelledExpr 
        (dum, "%break", LabelledExpr
           (p1,lbl,DoWhileExpr
              (p2, LabelledExpr 
                 (dum,"%continue",LabelledExpr
                    (dum,"%continue-"^lbl,stmt body)),
              expr test)))
  | S.DoWhileStmt (p, body, test) -> LabelledExpr
      (dum, "%break", WhileExpr 
           (p, LabelledExpr (dum, "%continue", stmt body),
            expr test))

  (* TODO: | S.DoWhileStmt (p,body,test) -> *)
  | S.BreakStmt a -> BreakExpr (a,"%break",UndefinedExpr dum)
  | S.BreakToStmt (a,lbl) -> BreakExpr (a,lbl,UndefinedExpr dum)
  | S.ContinueStmt a -> BreakExpr (a,"%continue",UndefinedExpr dum)
  | S.ContinueToStmt (a,lbl) -> BreakExpr (a,"%continue-"^lbl,UndefinedExpr dum)
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
        (forInit init)
        (LabelledExpr 
           (dum, "%break",
            WhileExpr 
              (dum, expr stop, 
               seq a
                 (LabelledExpr (dum, "%continue", stmt body))
                 (expr incr))))
  | S.VarDeclStmt (a, decls) -> varDeclList decls

and forInit (fi : S.forInit) = match fi with
    S.NoForInit -> UndefinedExpr dum
  | S.ExprForInit e -> expr e
  | S.VarForInit decls -> varDeclList decls

and varDeclList decls = match decls with
    [] -> UndefinedExpr dum
  | [d] -> varDecl d
  | d :: ds -> seq dum (varDecl d) (varDeclList ds)

and varDecl (decl : S.varDecl) = match decl with
    S.VarDeclNoInit (a, x) -> VarDeclExpr (a, x, UndefinedExpr dum)
  | S.VarDecl (a, x, e) -> VarDeclExpr (a, x, expr e)

and caseClauses (clauses : S.caseClause list) = match clauses with
    [] -> UndefinedExpr dum
  | (S.CaseDefault (a,s)::clauses) -> seq a (stmt s) (caseClauses clauses)
  | (S.CaseClause (a,e,s)::clauses) ->
      LetExpr (a,"%t",
               IfExpr (dum,VarExpr (dum,"%t"), 
                       (BoolExpr (dum,true)),
                       (expr e)),
               SeqExpr (dum,
                        IfExpr (dum,VarExpr (dum,"%t"),
                                stmt s,
                                UndefinedExpr dum),
                        caseClauses clauses))

and prop pr =  match pr with
    (p, S.PropId x,e) -> (p, x, expr e)
  | (p, S.PropString s,e) -> (p, s, expr e)
  | (p, S.PropNum n,e) -> (p, string_of_int n, expr e)


(** Generates an expression that evaluates and binds lv, then produces the
    the value of body_fn.  body_fn is applied with references to lv as an
    lvalue and lv as an expression. *)
and eval_lvalue (lv :  S.lvalue) (body_fn : lvalue * expr -> expr) =
  match lv with
    S.VarLValue (a,x) -> body_fn (VarLValue (a,x),VarExpr (dum,x))
  | S.DotLValue (a,e,x) -> 
      LetExpr (dum,"%lhs",expr e,
        body_fn (PropLValue (a,VarExpr (dum,"%lhs"),StringExpr (dum,x)),
                 BracketExpr (dum,VarExpr (dum,"%lhs"),StringExpr (dum,x))))
  | S.BracketLValue (a,e1,e2) -> 
      LetExpr (dum,"%lhs",expr e1,
      LetExpr (dum,"%field",expr e2,
      body_fn (PropLValue (a,VarExpr (dum,"%lhs"),VarExpr (dum,"%field")),
               BracketExpr (dum,VarExpr (dum,"%lhs"),VarExpr (dum,"%field")))))

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

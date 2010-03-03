module Main where

import BrownPLT.JavaScript
import System.Console.GetOpt
import System.IO
import System.Environment
import Text.ParserCombinators.Parsec.Pos (SourcePos)

import Control.Monad.State


--state is: (current position, whether we're in a function,
--           a hint for the name of a function,
--           filename of the code we're compiling)        
type CounterM a = State (Int, Bool, String, String) a


funcWrapperName = "__typedjs"
newWrapperName = "__new"
thisWrapperName = "__thisref"

prop :: (Prop SourcePos, Expression SourcePos) 
     -> CounterM (Prop SourcePos, Expression SourcePos) 
prop (p,e) = do
  e' <- expr e 
  return (p, e')


lvalue :: LValue SourcePos -> CounterM (LValue SourcePos)
lvalue (LVar p s) = return $ LVar p s
lvalue (LDot p e s) = do
  e' <- expr e
  return $ LDot p e' s
lvalue (LBracket p cont key) = do
  cont' <- expr cont
  key' <- expr key
  return $ LBracket p cont' key'
  
  
--get a name hint from an lvalue
lvalue2str (LVar p s) = s
lvalue2str (LDot p e s) = (renderExpression e) ++ "." ++ s
lvalue2str (LBracket p cont key) = ""


caseClause :: CaseClause SourcePos -> CounterM (CaseClause SourcePos)
caseClause (CaseClause p e ss) = do
  e' <- expr e
  ss' <- mapM stmt ss
  return $ CaseClause p e' ss'
caseClause (CaseDefault p ss) = do
  ss' <- mapM stmt ss
  return $ CaseDefault p ss'


varDecl :: VarDecl SourcePos -> CounterM (VarDecl SourcePos)
varDecl v@(VarDecl p name mEx) = case mEx of
  Just e -> do
    (a, b, nameHint, fn) <- get
    put (a, b, unId name, fn)
    e' <- expr e
    (a', b', _, fn) <- get
    put (a', b', nameHint, fn)
    return $ VarDecl p name (Just e')
  Nothing -> return v
  

forInit NoInit = return NoInit
forInit (VarInit decls) = do
  decls' <- mapM varDecl decls
  return $ VarInit decls
forInit (ExprInit e) = do
  e' <- expr e
  return $ ExprInit e'
  

catchClause (CatchClause p i s) = do
  s' <- stmt s
  return $ CatchClause p i s'


expr :: Expression SourcePos -> CounterM (Expression SourcePos)
expr e = case e of
  StringLit{} -> noop
  RegexpLit{} -> noop
  NumLit{} -> noop
  IntLit{} -> noop
  BoolLit{} -> noop
  NullLit{} -> noop
  ArrayLit p es -> do
    es' <- mapM expr es
    return $ ArrayLit p es'
  ObjectLit p props -> do
    props' <- mapM prop props
    return $ ObjectLit p props'
  ThisRef p -> 
    return $ CallExpr p (VarRef p (Id p thisWrapperName))
               [ThisRef p,
                DotRef p (VarRef p (Id p "arguments"))
                         (Id p "callee")]
  VarRef{} -> noop
  DotRef p e i -> do
    e' <- expr e
    return $ DotRef p e' i
  BracketRef p cont key -> do
    cont' <- expr cont
    key' <- expr key
    return $ BracketRef p cont' key'
  NewExpr p constr argEs -> do
    constr' <- expr constr
    argEs' <- mapM expr argEs

    return $ CallExpr p (VarRef p (Id p newWrapperName))
               [constr',
                ArrayLit p argEs']
  PrefixExpr p op e -> do
    e' <- expr e
    return $ PrefixExpr p op e'
  UnaryAssignExpr p op lval -> do
    lval' <- lvalue lval
    return $ UnaryAssignExpr p op lval'
  InfixExpr p op e1 e2 -> do 
    e1' <- expr e1
    e2' <- expr e2
    return $ InfixExpr p op e1' e2'
  CondExpr p c t f -> do
    c' <- expr c
    t' <- expr t
    f' <- expr f
    return $ CondExpr p c' t' f'
  AssignExpr p op lval e -> do
    lval' <- lvalue lval
    (a,b,nameHint, fn) <- get
    put (a,b,lvalue2str lval, fn)
    e' <- expr e
    (a',b',_, fn) <- get
    put (a',b',nameHint, fn)
    return $ AssignExpr p op lval' e'
  ParenExpr a e -> do
    e' <- expr e
    return $ ParenExpr a e'
  ListExpr a es -> do
    es' <- mapM expr es
    return $ ListExpr a es'
  CallExpr a func es -> do
    func' <- expr func
    es' <- mapM expr es
    return $ CallExpr a func' es'
  FuncExpr p mname argNames body -> do
    (n, isNested, nameHint, fn) <- get
    put (n + 1, isNested, nameHint, fn)
    return $ CallExpr p (VarRef p (Id p funcWrapperName))
               [FuncExpr p mname argNames (evalState (stmt body) (0, True, "", fn)),
                if isNested
                  then DotRef p (VarRef p (Id p "arguments"))  
                                (Id p "callee")
                  else VarRef p (Id p "undefined"),
                StringLit p $ maybe nameHint unId mname, 
                StringLit p $ fn,
                IntLit p n]
 where 
  noop = return e


stmt :: Statement SourcePos -> CounterM (Statement SourcePos)
stmt s = case s of
  BlockStmt p ss -> do
    ss' <- mapM stmt ss
    return $ BlockStmt p ss'
  EmptyStmt{} -> noop
  ExprStmt p e -> do
    e' <- expr e
    return $ ExprStmt p e'
  IfStmt p c t f -> do
    c' <- expr c
    t' <- stmt t
    f' <- stmt f
    return $ IfStmt p c' t' f'
  IfSingleStmt p c t -> do
    c' <- expr c
    t' <- stmt t
    return $ IfSingleStmt p c' t'
  SwitchStmt a e cases -> do
    e' <- expr e
    cases' <- mapM caseClause cases
    return $ SwitchStmt a e' cases'
  WhileStmt a e s -> do
    e' <- expr e
    s' <- stmt s
    return $ WhileStmt a e' s'
  DoWhileStmt a s e -> do
    s' <- stmt s
    e' <- expr e
    return $ DoWhileStmt a s' e'
  BreakStmt{} -> noop
  ContinueStmt{} -> noop
  LabelledStmt a i s -> do
    s' <- stmt s
    return $ LabelledStmt a i s'
  --the for in init cannot have any expressions/statements
  ForInStmt p init e s -> do
    e' <- expr e
    s' <- stmt s
    return $ ForInStmt p init e' s'
  ForStmt a init mtest minc body -> do
    init' <- forInit init
    mtest' <- maybe mtest expr
    minc' <- maybe minc expr
    body' <- stmt body
    return $ ForStmt a init' mtest' minc' body'
  TryStmt a s catches mfinally -> do
    s' <- stmt s
    catches' <- mapM catchClause catches
    mfinally' <- maybe mfinally stmt
    return $ TryStmt a s' catches' mfinally'
  ThrowStmt a e -> do
    e' <- expr e
    return $ ThrowStmt a e'
  ReturnStmt a me -> do
    me' <- maybe me expr
    return $ ReturnStmt a me'
  WithStmt a e s -> do 
   e' <- expr e
   s' <- stmt s
   return $ WithStmt a e' s'
  VarDeclStmt p decls -> do
    decls' <- mapM varDecl decls 
    return $ VarDeclStmt p decls'
  FunctionStmt p (Id pp name) args body -> do
    --we don't want the funcexprs to be named, since any references
    --to the function should be to the wrapped one.
    let func = FuncExpr p Nothing args body
    --let fexpr = ExprStmt p (AssignExpr p OpAssign (LVar p name) func)
    let fexpr = VarDeclStmt p [VarDecl p (Id pp name) (Just func)]
    stmt fexpr
 where
  noop = return s
  maybe m f = case m of
    Nothing -> return Nothing
    Just x -> do
      x' <- f x
      return $ Just x'
{-
instrumentFuncExpr :: String
                   -> Expression SourcePos -> Expression SourcePos
instrumentFuncExpr instrName fn@(FuncExpr p args body) =
  CallExpr p (VarRef p (Id p instrName)) [StringLit p (show p), fn]
instrumentFuncExpr _ e = e
-}

main :: IO ()
main = do
  args <- getArgs
  script <- parseJavaScriptFromFile (head args)
  let script' = evalState (mapM stmt script) (0, False, "", head args)
  putStrLn (renderStatements script')

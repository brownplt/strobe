module Main where

import BrownPLT.JavaScript
import BrownPLT.JavaScript.Crawl ()
import Data.Generics
import System.Console.GetOpt
import System.IO
import System.Environment
import Text.ParserCombinators.Parsec.Pos (SourcePos)

removeFuncStmt :: Statement SourcePos -> Statement SourcePos
removeFuncStmt (FunctionStmt p name args body) =
  ExprStmt p (AssignExpr p OpAssign (VarRef p name) func)
    where func = FuncExpr p args body
removeFuncStmt s = s

instrumentFuncExpr :: String
                   -> Expression SourcePos -> Expression SourcePos
instrumentFuncExpr instrName fn@(FuncExpr p args body) =
  CallExpr p (VarRef p (Id p instrName)) [StringLit p (show p), fn]
instrumentFuncExpr _ e = e

main :: IO ()
main = do
  args <- getArgs
  script <- parseJavaScriptFromFile (head args)
  let script' = everywhere (mkT (instrumentFuncExpr "__typedjs")) $
                everywhere (mkT removeFuncStmt) script
  
  putStrLn (renderStatements script')

open Prelude
open JavaScript_syntax

let expr_annotation (e : expr) = match e with
    StringExpr (a, _) -> a
  | RegexpExpr (a, _, _, _) -> a
  | NumExpr (a, _) -> a
  | IntExpr (a, _) -> a
  | BoolExpr (a, _) -> a
  | NullExpr a -> a
  | ArrayExpr (a, _) -> a
  | ObjectExpr (a, _) -> a
  | ThisExpr a -> a
  | VarExpr (a, _) -> a
  | DotExpr (a, _, _) -> a
  | BracketExpr (a, _, _) -> a
  | NewExpr (a, _, _) -> a
  | PrefixExpr (a, _, _) -> a
  | UnaryAssignExpr (a, _, _) -> a
  | InfixExpr (a, _, _, _) -> a
  | IfExpr (a, _, _, _) -> a
  | AssignExpr (a, _, _, _) -> a
  | ParenExpr (a, _) -> a
  | ListExpr (a, _, _) -> a
  | CallExpr (a, _, _) -> a
  | FuncExpr (a, _, _) -> a
  | NamedFuncExpr (a, _, _, _) -> a
  | UndefinedExpr a -> a

let stmt_annotation (s : stmt) = match s with
    BlockStmt (a, _) -> a
  | EmptyStmt a -> a  
  | ExprStmt e -> expr_annotation e
  | IfStmt (a, _, _, _) -> a
  | IfSingleStmt (a, _, _) -> a
  | SwitchStmt (a, _, _) -> a
  | WhileStmt (a, _, _) -> a
  | DoWhileStmt (a, _, _) -> a
  | BreakStmt a -> a
  | BreakToStmt (a, _) -> a
  | ContinueStmt a -> a
  | ContinueToStmt (a, _) -> a
  | LabelledStmt (a, _, _) -> a
  | ForInStmt (a, _, _, _) -> a
  | ForStmt (a, _, _, _, _) -> a
  | TryStmt (a, _, _, _) -> a
  | ThrowStmt (a, _) -> a
  | ReturnStmt (a, _) -> a
  | WithStmt _ -> 
      (* INFO: Looks like I forgot to add annotations to WithStmt. Who cares,
         really? *)
      failwith "with statements are not annotated"
  | VarDeclStmt (a, _) -> a
  | FuncStmt (a, _, _, _) -> a

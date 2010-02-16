open Prelude

type 'a stmt 
  = BlockStmt of 'a * ('a stmt list)
  | EmptyStmt of 'a  
  | ExprStmt of 'a expr
  | IfStmt of 'a * ('a expr) * ('a stmt) * ('a stmt)
  | IfSingleStmt of 'a * ('a expr) * ('a stmt)
  | SwitchStmt of 'a * 'a expr * 'a caseClause list
  | WhileStmt of 'a * 'a expr * 'a stmt
  | DoWhileStmt of 'a * 'a stmt * 'a expr
  | BreakStmt of 'a
  | BreakToStmt of 'a * id
  | ContinueStmt of 'a
  | ContinueToStmt of 'a * id
  | LabelledStmt of 'a * id * 'a stmt
  | ForInStmt of 'a * 'a forInInit * 'a expr * 'a stmt
  | ForStmt of 'a * 'a forInit * 'a expr * 'a expr * 'a stmt
  | TryStmt of 'a * 'a stmt * 'a catch list * 'a stmt
  | ThrowStmt of 'a * 'a expr
  | ReturnStmt of 'a * 'a expr
  | WithStmt of 'a expr * 'a stmt
  | VarDeclStmt of 'a * 'a varDecl list
  | FuncStmt of 'a * id * id list * 'a stmt

and 'a varDecl
  = VarDeclNoInit of 'a * id
  | VarDecl of 'a * id * 'a expr

and 'a forInit
  = NoForInit
  | VarForInit of 'a varDecl list
  | ExprForInit of 'a expr

and 'a forInInit
 = VarForInInit of 'a * id
 | NoVarForInInit of 'a * id

and 'a catch
  = CatchClause of 'a * id * 'a stmt

and 'a caseClause
  = CaseClause of 'a * 'a expr * 'a stmt
  | CaseDefault of 'a * 'a stmt

and 'a expr 
  = StringExpr of 'a * string
  | RegexpExpr of 'a * string * bool * bool
  | NumExpr of 'a * float
  | IntExpr of 'a * int
  | BoolExpr of 'a * bool
  | NullExpr of 'a
  | ArrayExpr of 'a * 'a expr list
  | ObjectExpr of 'a * ('a * prop * 'a expr) list
  | ThisExpr of 'a
  | VarExpr of 'a * id
  | DotExpr of 'a * 'a expr * id
  | BracketExpr of 'a * 'a expr * 'a expr
  | NewExpr of 'a * 'a expr * 'a expr list
  | PrefixExpr of 'a * prefixOp * 'a expr
  | UnaryAssignExpr of 'a * unaryAssignOp * 'a lvalue
  | InfixExpr of 'a * infixOp * 'a expr * 'a expr
  | IfExpr of 'a * 'a expr * 'a expr * 'a expr
  | AssignExpr of 'a * assignOp * 'a lvalue * 'a expr
  | ParenExpr of 'a * 'a expr
  | ListExpr of 'a * 'a expr * 'a expr
  | CallExpr of 'a * 'a expr * 'a expr list
  | FuncExpr of 'a * id list * 'a stmt
  | NamedFuncExpr of 'a * id * id list * 'a stmt
  | UndefinedExpr of 'a

and 'a lvalue
  = VarLValue of 'a * id
  | DotLValue of 'a * 'a expr * id
  | BracketLValue of 'a * 'a expr * 'a expr

and prefixOp
  = PrefixLNot 
  | PrefixBNot 
  | PrefixPlus
  | PrefixMinus 
  | PrefixTypeof 
  | PrefixVoid 
  | PrefixDelete

and unaryAssignOp
  = PrefixInc 
  | PrefixDec 
  | PostfixInc 
  | PostfixDec


and infixOp
  = OpLT 
  | OpLEq 
  | OpGT 
  | OpGEq  
  | OpIn
  | OpInstanceof
  | OpEq
  | OpNEq
  | OpStrictEq
  | OpStrictNEq
  | OpLAnd
  | OpLOr 
  | OpMul
  | OpDiv
  | OpMod
  | OpSub
  | OpLShift
  | OpSpRShift
  | OpZfRShift
  | OpBAnd
  | OpBXor
  | OpBOr
  | OpAdd

and assignOp
  =  OpAssign
  | OpAssignAdd
  | OpAssignSub
  | OpAssignMul
  | OpAssignDiv
  | OpAssignMod
  | OpAssignLShift
  | OpAssignSpRShift
  | OpAssignZfRShift
  | OpAssignBAnd
  | OpAssignBXor
  | OpAssignBOr

and prop 
  = PropId of id
  | PropString of string
  | PropNum of int

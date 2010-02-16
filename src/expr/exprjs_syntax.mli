(** Simplifies the syntax of JavaScript, primarily by removing statements.
    To remove statements, we have to introduce a few new control operators as
    expressions. *)

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

val from_javascript : JavaScript_syntax.stmt list -> expr

val from_javascript_expr : JavaScript_syntax.expr -> expr

val locals : expr -> IdSet.t

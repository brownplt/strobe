(** Simplifies the syntax of JavaScript, primarily by removing statements.
    To remove statements, we have to introduce a few new control operators as
    expressions. *)

open Prelude

type 'a expr
  = StringExpr of 'a * string
  | RegexpExpr of 'a * string * bool * bool
  | NumExpr of 'a * float
  | IntExpr of 'a * int
  | BoolExpr of 'a * bool
  | NullExpr of 'a
  | ArrayExpr of 'a * 'a expr list
  | ObjectExpr of 'a * (string * 'a expr) list
      (** Object properties are transformed into string literals *)
  | ThisExpr of 'a
  | VarExpr of 'a * id
  | BracketExpr of 'a * 'a expr * 'a expr
  | NewExpr of 'a * 'a expr * 'a expr list
  | PrefixExpr of 'a * JavaScript_syntax.prefixOp * 'a expr
  | InfixExpr of 'a * JavaScript_syntax.infixOp * 'a expr * 'a expr
  | IfExpr of 'a * 'a expr * 'a expr * 'a expr
  | AssignExpr of 'a * 'a lvalue * 'a expr
  | AppExpr of 'a * 'a expr * 'a expr list
  | FuncExpr of 'a * id list * 'a expr
  | UndefinedExpr of 'a
  | LetExpr of 'a * id * 'a expr * 'a expr
      (** We need let-expressions to simplify statements. *)
  | SeqExpr of 'a * 'a expr * 'a expr
  | WhileExpr of 'a * 'a expr * 'a expr
  | LabelledExpr of 'a * id * 'a expr
  | BreakExpr of 'a * id * 'a expr
  | ForInExpr of 'a * id * 'a expr * 'a expr
  | VarDeclExpr of 'a * id * 'a expr
      (** We do not transform VarDeclStmts to let-bindings at this stage *)
  | TryCatchExpr of 'a * 'a expr * id * 'a expr
  | TryFinallyExpr of 'a * 'a expr * 'a expr
  | ThrowExpr of 'a * 'a expr
  | FuncStmtExpr of 'a * id * id list * 'a expr
      (** We leave function statements in place, so that they can be lifted
          for JavaScript to turned into letrecs for Typed JavaScript. *)


and 'a lvalue =
    VarLValue of 'a * id
  | PropLValue of 'a * 'a expr * 'a expr

val from_javascript : pos JavaScript_syntax.stmt list -> pos expr

val locals : 'a expr -> IdSet.t

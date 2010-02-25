open Prelude
open FormatExt

type op1 = Op1Prefix of JavaScript_syntax.prefixOp

(* TODO: unchecked operations should always use differnet syntax. add an
   uncheckedGetField, uncheckedSetField, updateField, App, and if, ? *)
type op2 =
    Op2Infix of JavaScript_syntax.infixOp

type exp =
    EConst of pos * Exprjs_syntax.const
  | EId of pos * id
  | EArray of pos * exp list
  | EObject of pos * (pos * string * exp) list
  | EGetField of pos * exp * exp
  | EDeleteField of pos * exp * exp
  | EUpdateField of pos * exp * exp * exp
  | EOp1 of pos * op1 * exp
  | EOp2 of pos * op2 * exp * exp
  | EIf of pos * exp * exp * exp
  | EApp of pos * exp * exp list
  | ERef of pos * exp
  | EDeref of pos * exp
  | ESetRef of pos * exp * exp
  | ESeq of pos * exp * exp
  | ELet of pos * id * exp * exp
  | EFix of pos * (id * exp) list * exp 
      (** All bindings must be [ELambda]s. *)
  | ELabel of pos * id * exp
  | EBreak of pos * id * exp
  | ETryCatch of pos * exp * exp
      (** Catch block must be an [ELambda] *)
  | ETryFinally of pos * exp * exp
  | EThrow of pos * exp
  | ELambda of pos * id list * exp

val desugar : Exprjs_syntax.expr -> exp

val p_op1 : op1 -> printer

val p_op2 : op2 -> printer

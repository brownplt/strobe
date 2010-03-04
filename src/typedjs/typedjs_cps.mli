open Prelude
open Typedjs_syntax 

type cpsval =
    Const of Exprjs_syntax.const
  | Id of pos * id

type node = int

type op1 = 
  | Op1Prefix of JavaScript_syntax.prefixOp
  | Deref
  | Ref

type op2 =
  | Op2Infix of JavaScript_syntax.infixOp
  | GetField
  | DeleteField
  | SetRef

type bindexp =
  | Let of cpsval
  | Op1 of op1 * cpsval
  | Op2 of op2 * cpsval * cpsval
  | Object of (string * cpsval) list
  | Array of cpsval list
  | UpdateField of cpsval * cpsval * cpsval

type cpsexp =
    Fix of node * (id * id list * typ * cpsexp) list * cpsexp
  | App of node * cpsval * cpsval list
  | If of node * cpsval * cpsexp * cpsexp
  | Bind of node * id * bindexp * cpsexp

val cps : exp -> cpsexp

val node_of_cpsexp : cpsexp -> node

val p_cpsexp : cpsexp -> FormatExt.printer

(** [esc_cpsexp e] returns names of the escaping functions of [e]. [e] must
    follow the unique binding convention. *)
val esc_cpsexp : cpsexp -> IdSet.t

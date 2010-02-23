open Prelude
open Typedjs_syntax 

type cpsval =
    Const of const
  | Id of id
  | Array of cpsval list
  | Object of (id * cpsval) list

type node = int

type cpsexp =
    Fix of node * (id * id list * typ * cpsexp) list * cpsexp
  | App of node * cpsval * cpsval list
  | If of node * cpsval * cpsexp * cpsexp
  | Let0 of node * id * cpsval * cpsexp (* load immediate / reg-reg move *)
  | Let1 of node * id * JavaScript_syntax.prefixOp * cpsval * cpsexp
  | Let2 of node * id * JavaScript_syntax.infixOp * cpsval * cpsval * cpsexp
  | Assign of node * id * cpsval * cpsexp
  | SetProp of node * cpsval * cpsval * cpsval * cpsexp

val cps : exp -> cpsexp

val p_cpsexp : cpsexp -> FormatExt.printer

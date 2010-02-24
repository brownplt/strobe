open Prelude
open Typedjs_syntax 

type cpsval =
    Const of Exprjs_syntax.const
  | Id of id

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
  | Array of node * id * cpsval list * cpsexp
  | Object of node * id * (cpsval * cpsval) list * cpsexp
  | GetField of node * id * cpsval * cpsval * cpsexp

val cps : exp -> cpsexp

val node_of_cpsexp : cpsexp -> node

val p_cpsexp : cpsexp -> FormatExt.printer

(** [esc_cpsexp e] returns names of the escaping functions of [e]. [e] must
    follow the unique binding convention. *)
val esc_cpsexp : cpsexp -> IdSet.t

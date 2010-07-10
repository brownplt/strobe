open Prelude
open Typedjs_syntax 

type cpsval =
    Const of JavaScript_syntax.const
  | Id of pos * id

type node = int

type op1 = 
  | Op1Prefix of id
  | Deref
  | Ref

type op2 =
  | Op2Infix of id
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
    Fix of node * (bool * id * id list * typ * cpsexp) list * cpsexp
  | App of node * cpsval * cpsval list
  | If of node * cpsval * cpsexp * cpsexp
  | Bind of node * id * bindexp * cpsexp

val cps : def -> cpsexp

val simpl_cps : def -> cpsexp

val node_of_cpsexp : cpsexp -> node

val p_cpsexp : cpsexp -> FormatExt.printer

val fv_cpsexp : cpsexp -> IdSet.t

(** [esc_cpsexp e] returns names of the escaping functions of [e]. [e] must
    follow the unique binding convention. *)
val esc_cpsexp : cpsexp -> IdSet.t

val subst : id -> cpsval -> cpsexp -> cpsexp


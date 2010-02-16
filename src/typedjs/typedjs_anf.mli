open Prelude
open Typedjs_syntax

type node = int

module NodeMap : Map.S
  with type key = node

module NodeMapExt : MapExt.S
  with type key = node
  and type +'a t = 'a NodeMap.t

type value =
    VId of pos * id
  | VString of string
  | VNum of float
  | VInt of int
  | VRegexp of string * bool * bool
  | VBool of bool
  | VNull
  | VArray of value list
  | VObject of (string * value) list
  | VThis
  | VFunc of id list * typ * anfexp
  | VUndefined

and bind =
  | BValue of value
  | BApp of value * value list
  | BBracket of value * value
  | BNew of value * value list
  | BPrefixOp of JavaScript_syntax.prefixOp * value
  | BInfixOp of JavaScript_syntax.infixOp * value * value
  | BAssign of id * value
  | BSetProp of value * value * value
  | BIf of value * anfexp * anfexp
  | BTryCatch of anfexp * anfexp
  | BTryFinally of anfexp * anfexp


and anfexp =
    ALet of node * id * bind * anfexp
  | ARec of node * (id * bind) list * anfexp
  | ALabel of node * id * anfexp
  | ABreak of node * id * value
  | AThrow of node * value
  | AValue of node * value

val from_typedjs : exp -> anfexp

val pretty_anfexp : Format.formatter -> anfexp -> unit

val print_anfexp : anfexp -> unit

val node_of_anfexp : anfexp -> node

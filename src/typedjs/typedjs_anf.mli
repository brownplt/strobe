open Prelude
open Typedjs_syntax

type node = int

module NodeMap : Map.S
  with type key = node

module NodeMapExt : MapExt.S
  with type key = node
  and type +'a t = 'a NodeMap.t

type 'a value =
    VId of 'a * id
  | VString of string
  | VNum of float
  | VInt of int
  | VRegexp of string * bool * bool
  | VBool of bool
  | VNull
  | VArray of 'a value list
  | VObject of (string * 'a value) list
  | VThis
  | VFunc of id list * typ * 'a anfexp
  | VUndefined

and 'a bind =
  | BValue of 'a value
  | BApp of 'a value * 'a value list
  | BBracket of 'a value * 'a value
  | BNew of 'a value * 'a value list
  | BPrefixOp of JavaScript_syntax.prefixOp * 'a value
  | BInfixOp of JavaScript_syntax.infixOp * 'a value * 'a value
  | BAssign of id * 'a value
  | BSetProp of 'a value * 'a value * 'a value
  | BIf of 'a value * 'a anfexp * 'a anfexp


and 'a anfexp =
    ALet of node * id * 'a bind * 'a anfexp
  | ARec of node * (id * 'a bind) list * 'a anfexp
  | ALabel of node * id * 'a anfexp
  | ABreak of node * id * 'a value
  | ATryCatch of node * 'a anfexp * id * 'a anfexp
  | ATryFinally of node * 'a anfexp * 'a anfexp
  | AThrow of node * 'a value
  | AValue of node * 'a value

val from_typedjs : 'a exp -> 'a anfexp

val pretty_anfexp : Format.formatter -> 'a anfexp -> unit

val print_anfexp : 'a anfexp -> unit

val node_of_anfexp : 'a anfexp -> node

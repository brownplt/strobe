open Prelude

type runtime_typ =
    RTNumber
  | RTString
  | RTBoolean
  | RTFunction
  | RTObject
  | RTUndefined

module RTSet : Set.S
  with type elt = runtime_typ

module RTSetExt : SetExt.S
  with type elt = runtime_typ
  and type t = RTSet.t

type abs_value =
    AVType of RTSet.t
  | AVTypeof of id
  | AVString of string
  | AVTypeIs of id * RTSet.t

type runtime_typs = RTSet.t

type constr = string

type typ = 
    TApp of constr * typ list
  | TUnion of typ * typ
  | TArrow of typ * typ list * typ
  | TObject of (id * typ) list
  | TRef of typ
  | TTop
  | TBot


type annotation =
    ATyp of typ
  | AConstructor of typ 
  | AMutable

(** Typed JavaScript expressions. Additional well-formedness criteria are
    inline. *)
type exp
  = EString of pos * string
  | ERegexp of pos * string * bool * bool
  | ENum of pos * float
  | EInt of pos * int
  | EBool of pos * bool
  | ENull of pos
  | EArray of pos * exp list
  | EObject of pos * (string * bool * exp) list
      (* [Typedjs_fromExpr.from_exprjs] ensures that the field names are 
         unique. If the [bool] on a field is true, the field is mutable. *)
  | EThis of pos
  | EId of pos * id
  | EBracket of pos * exp * exp
  | ENew of pos * exp * exp list
  | EPrefixOp of pos * JavaScript_syntax.prefixOp * exp
  | EInfixOp of pos * JavaScript_syntax.infixOp * exp * exp
  | EIf of pos * exp * exp * exp
  | EAssign of pos * lvalue * exp
  | EApp of pos * exp * exp list
  | EFunc of pos * id list * typ * exp
      (* [Typedjs_fromExpr.from_exprjs] ensures that the argument names are
         unique. *)
  | EUndefined of pos
  | ELet of pos * id * exp * exp
  | ERec of (id * typ * exp) list * exp
  | ESeq of pos * exp * exp
  | ELabel of pos * id * typ * exp 
      (** A labelled jump has a type-annotation to aid the type-checker. Without
          the annotation, we would have to union the types of all [EBreak]s to
          each label. *)
  | EBreak of pos * id * exp
  | ETryCatch of pos * exp * id * exp
  | ETryFinally of pos * exp * exp
  | EThrow of pos * exp
  | ETypecast of pos * runtime_typs * exp

and lvalue =
    LVar of pos * id
  | LProp of pos * exp * exp


module type EnvType = sig
  
  type env

  val empty_env : env

  val bind_id : id -> typ -> env -> env

  val bind_lbl : id -> typ -> env -> env

  val lookup_id : id -> env -> typ

  val lookup_lbl : id -> env -> typ

  val assignable_ids : env -> IdSet.t

  val new_assignable_id : id -> env -> env

  val remove_assigned_ids : IdSet.t -> env -> env

  val id_env : env -> typ IdMap.t

end

module Env : EnvType

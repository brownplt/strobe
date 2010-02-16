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
  | TTop
  | TBot


type annotation =
    ATyp of typ
  | AConstructor of typ 

type 'a exp
  = EString of 'a * string
  | ERegexp of 'a * string * bool * bool
  | ENum of 'a * float
  | EInt of 'a * int
  | EBool of 'a * bool
  | ENull of 'a
  | EArray of 'a * 'a exp list
  | EObject of 'a * (string * 'a exp) list
  | EThis of 'a
  | EId of 'a * id
  | EBracket of 'a * 'a exp * 'a exp
  | ENew of 'a * 'a exp * 'a exp list
  | EPrefixOp of 'a * JavaScript_syntax.prefixOp * 'a exp
  | EInfixOp of 'a * JavaScript_syntax.infixOp * 'a exp * 'a exp
  | EIf of 'a * 'a exp * 'a exp * 'a exp
  | EAssign of 'a * 'a lvalue * 'a exp
  | EApp of 'a * 'a exp * 'a exp list
  | EFunc of 'a * id list * typ * 'a exp
  | EUndefined of 'a
  | ELet of 'a * id * 'a exp * 'a exp
  | ERec of (id * typ * 'a exp) list * 'a exp
  | ESeq of 'a * 'a exp * 'a exp
  | ELabel of 'a * id * typ * 'a exp 
      (** A labelled jump has a type-annotation to aid the type-checker. Without
          the annotation, we would have to union the types of all [EBreak]s to
          each label. *)
  | EBreak of 'a * id * 'a exp
  | ETryCatch of 'a * 'a exp * id * 'a exp
  | ETryFinally of 'a * 'a exp * 'a exp
  | EThrow of 'a * 'a exp
  | ETypecast of 'a * runtime_typs * 'a exp

and 'a lvalue =
    LVar of 'a * id
  | LProp of 'a * 'a exp * 'a exp

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

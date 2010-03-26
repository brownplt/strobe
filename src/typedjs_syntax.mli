open Prelude

exception Typ_error of pos * string

module RT : sig
  type t =
    | Number
    | String
    | Boolean
    | Function
    | Object
    | Undefined

  val compare : t -> t -> int

  val pp : t -> FormatExt.printer

end

module RTSet : Set.S
  with type elt = RT.t

module RTSetExt : SetExt.S
  with type elt = RT.t
  and type t = RTSet.t

type constr = string

type typ = 
    TApp of constr * typ list
  | TUnion of typ * typ
  | TArrow of typ * typ list * typ      
  | TObject of (id * typ) list
  | TRef of typ
  | TSource of typ
  | TSink of typ
  | TTop
  | TBot

type env_decl =
    EnvClass of constr * typ * (id * typ) list
  | EnvBind of id * typ

type annotation =
    ATyp of typ
  | AConstructor of typ 
  | AUpcast of typ
  | ADowncast of typ

(** Typed JavaScript expressions. Additional well-formedness criteria are
    inline. *)
type exp
  = EConst of pos * JavaScript_syntax.const
  | EArray of pos * exp list
  | EObject of pos * (string * exp) list
      (* [Typedjs_fromExpr.from_exprjs] ensures that field names are unique. *)
  | EThis of pos
  | EId of pos * id
  | EBracket of pos * exp * exp
  | EUpdateField of pos * exp * exp * exp
  | ENew of pos * id * exp list
  | EPrefixOp of pos * JavaScript_syntax.prefixOp * exp
  | EInfixOp of pos * JavaScript_syntax.infixOp * exp * exp
  | EIf of pos * exp * exp * exp
  | EApp of pos * exp * exp list
  | EFunc of pos * id list * typ * exp
      (* [Typedjs_fromExpr.from_exprjs] ensures that the argument names are
         unique. *)
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
  | ETypecast of pos * RTSet.t * exp
  | ERef of pos * exp
  | EDeref of pos * exp
  | ESetRef of pos * exp * exp
  | ESubsumption of pos * typ * exp
  | EDowncast of pos * typ * exp

type constr_exp = { 
  constr_pos : pos;
  constr_name : id;
  constr_typ : typ;
  constr_args : id list;
  constr_inits : (id * exp) list;
  constr_exp : exp;
  constr_prototype : exp
}

(** Module-level definitions *)
type def =
    DEnd
  | DExp of exp * def
  | DLet of pos * id * exp * def
  | DRec of (id * typ * exp) list * def
  | DConstructor of constr_exp * def
      (* p, constr name, field name, field type, field expr, rest of defs *)
  | DExternalMethod of pos * id * id * exp * def

module Exp : sig

  type t = exp

  val pos : exp -> pos

end

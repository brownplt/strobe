open Prelude

exception Typ_error of pos * string

module RT : sig
  type t =
    | Num
    | Str
    | Bool
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

type prim =
  | Num
  | Int
  | Str
  | True
  | False
  | Undef
  | Null

type field = RegLang_syntax.regex * RegLang.fsm

type typ = 
  | TPrim of prim
  | TUnion of typ * typ
  | TIntersect of typ * typ
  | TArrow of typ list * typ      
  | TObject of (field * typ) list
  | TRegex of field
  | TRef of typ
  | TSource of typ
  | TSink of typ
  | TTop
  | TBot
  | TForall of id * typ * typ (** [TForall (a, s, t)] forall a <: s . t *)
  | TId of id
  | TField
  | TRec of id * typ 
  | TSyn of id (** type synonym *)

val typ_bool : typ

type env_decl =
  | EnvClass of constr * constr option * typ
  | EnvBind of id * typ

type annotation =
    ATyp of typ
  | AConstructor of typ 
  | AUpcast of typ
  | ADowncast of typ
  | ATypAbs of id * typ
  | ATypApp of typ
  | AAssertTyp of typ
  | ACheat of typ

type ref_kind =
  | RefCell
  | SourceCell
  | SinkCell

(** Typed JavaScript expressions. Additional well-formedness criteria are
    inline. *)
type exp =
  | EConst of pos * JavaScript_syntax.const
  | EBot of pos
  | EAssertTyp of pos * typ * exp
  | EArray of pos * exp list
  | EEmptyArray of pos * typ
  | EObject of pos * (string * exp) list
      (* [Typedjs_fromExpr.from_exprjs] ensures that field names are unique. *)
  | EId of pos * id
  | EBracket of pos * exp * exp
  | EUpdate of pos * exp * exp * exp
  | ENew of pos * id * exp list
  | EPrefixOp of pos * id * exp
  | EInfixOp of pos * id * exp * exp
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
  | ERef of pos * ref_kind * exp
  | EDeref of pos * exp
  | ESetRef of pos * exp * exp
  | ESubsumption of pos * typ * exp
  | EDowncast of pos * typ * exp
  | ETypAbs of pos * id * typ * exp 
      (** [ETypAbs (_, x, t, e)] Lambda x <: t . e *)
  | ETypApp of pos * exp * typ
      (** [ETypApp (_, e, t)] e t *)
  | EForInIdx of pos
  | ECheat of pos * typ * exp

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

module Typ : sig

  (* [match_func_typ typ] returns the list of argument types and the return
     type, if [typ] is a function. Note that function types have additional
     components. (The type of [this], quantifiers, etc.) *)
  val match_func_typ : typ -> (typ list * typ) option

end

module Pretty : sig

  open Format
  open FormatExt

  val p_typ : typ -> printer
  val p_exp : exp -> printer
  val p_def : def -> printer

  val pp_typ : formatter -> typ -> unit

end

val string_of_typ : typ -> string


open Prelude

exception Typ_error of pos * string

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
  | TDom

type env_decl =
    EnvClass of constr * typ * (id * typ) list
  | EnvBind of id * typ

type annotation =
    ATyp of typ
  | AConstructor of typ 
  | AMutable
  | AInferred of annotation list

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

  
  
(*
  | DConstructor of pos * id * typ * (id * exp) list * exp * exp
      (** [DConstructor pos c_name c_typ c_inits c_body prototype] is an object
          constructor (a function with a constructor annotation). A constructor
          begins by initializing its fields in a sequence of [this.x = e]
          expressions. [c_inits] is the initialization sequence, and [c_body]
          is the remainder of the constructor. In the initialization 
          expressions, the fields that are being added cannot be used. *)
  | DExternalField of pos * id * id * exp
      (** [DExternalField pos c_name f_name e] represents a statement of the
          form [c_name.prototype.f_name = e;]. *)
  | DExternalMethods of (pos * id * id * typ * exp) list
      (** [DExternalMethods method_decls] represents a sequence of external
          methods definitions. *)
*)          

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

  (** JavaScript cannot perform a labelled jump across a function. *)
  val clear_labels : env -> env

  val dom : env -> IdSet.t

  (** A new class with no methods. *)
  val new_class : id -> env -> env


  (** Adds a method to a class. *)
  val add_method : id -> id -> typ -> env -> env



end

module Env : EnvType

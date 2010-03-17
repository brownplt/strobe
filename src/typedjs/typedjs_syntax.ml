open Prelude

exception Typ_error of pos * string

module RT = struct
  type t =
    | Number
    | String
    | Boolean
    | Function
    | Object
    | Undefined

  let compare = Pervasives.compare

  open FormatExt

  let pp v = match v with
    | Number -> text "number"
    | String -> text "string"
    | Boolean -> text "boolean"
    | Function -> text "function"
    | Object -> text "object"
    | Undefined -> text "undefined"

end

module RTSet = Set.Make (RT)
module RTSetExt = SetExt.Make (RTSet)

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
  = EConst of pos * Exprjs_syntax.const
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

type constr_exp = { 
  constr_pos : pos;
  constr_name : id;
  constr_typ : typ;
  constr_args : id list;
  constr_inits : (id * exp) list;
  constr_exp : exp;
  constr_prototype : exp
}

type def =
    DEnd
  | DExp of exp * def
  | DLet of pos * id * exp * def
  | DRec of (id * typ * exp) list * def
  | DConstructor of constr_exp * def
  | DExternalMethod of pos * id * id * exp * def

(******************************************************************************)

module Env = struct

  type env = { id_typs : typ IdMap.t; 
               lbl_typs : typ IdMap.t;
               (* maps class names to a structural object type *)
               classes : typ IdMap.t 
             }


  let empty_env = { 
    id_typs = IdMap.empty;
    lbl_typs = IdMap.empty;
    classes = IdMap.empty
  }

  let bind_id x t env  = { env with id_typs = IdMap.add x t env.id_typs }

  let bind_lbl x t env = { env with lbl_typs = IdMap.add x t env.lbl_typs }

  let lookup_id x env = IdMap.find x env.id_typs

  let lookup_lbl x env = IdMap.find x env.lbl_typs

  let lookup_class x env = IdMap.find x env.classes

  let id_env env = env.id_typs

  let get_classes env = env.classes (* ocaml sucks *)

  let clear_labels env = { env with lbl_typs = IdMap.empty }

  let dom env = IdSetExt.from_list (IdMapExt.keys env.id_typs)

  let new_class class_name env = 
    if IdMap.mem class_name env.classes then
      raise (Invalid_argument ("class already exists: " ^ class_name))
    else 
      { env with
          classes = IdMap.add class_name (TObject []) env.classes
      }


  let add_method class_name method_name method_typ env =
    let class_typ = IdMap.find class_name env.classes in
      match class_typ with
          TObject fields ->
            if List.mem_assoc method_name fields then
              raise (Invalid_argument ("method already exists: " ^ method_name))
            else
              let class_typ' = TObject ((method_name, method_typ) :: fields) in
                { env with classes = IdMap.add class_name class_typ'
                    (* (Typedjs_types.typ_permute class_typ') *) env.classes }
        | _ ->
            failwith ("class type is not an object: " ^ class_name)


end

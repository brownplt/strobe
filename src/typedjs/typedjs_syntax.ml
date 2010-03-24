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
  | TSource of typ
  | TSink of typ
  | TTop
  | TBot
  | TDom

type env_decl =
    EnvClass of constr * typ * (id * typ) list
  | EnvBind of id * typ

type annotation =
    ATyp of typ
  | AConstructor of typ 
  | AInferred of annotation list
  | AUpcast of typ
  | ADowncast of typ

(** Typed JavaScript expressions. Additional well-formedness criteria are
    inline. *)
type exp
  = EConst of pos * JavaScript_syntax.const
  | EArray of pos * exp list
  | EObject of pos * (string * exp) list
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
  | ELet of pos * id * exp * exp
  | ERec of (id * typ * exp) list * exp
  | ESeq of pos * exp * exp
  | ELabel of pos * id * typ * exp 
  | EBreak of pos * id * exp
  | ETryCatch of pos * exp * id * exp
  | ETryFinally of pos * exp * exp
  | EThrow of pos * exp
  | ETypecast of pos * RTSet.t * exp
  | ERef of pos * exp
  | EDeref of pos * exp
  | ESetRef of pos * exp * exp
  | ESubsumption of pos * typ * exp
  | EParens of pos * exp
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

type def =
    DEnd
  | DExp of exp * def
  | DLet of pos * id * exp * def
  | DRec of (id * typ * exp) list * def
  | DConstructor of constr_exp * def
  | DExternalMethod of pos * id * id * exp * def

(******************************************************************************)

module Exp = struct

  type t = exp

  let children exp = match exp with
    | EConst _ -> []
    | EArray (_, es) -> es 
    | EObject (_, ps) -> map snd2 ps
    | EThis _ -> []
    | EId _ -> []
    | EBracket (_, e1, e2) -> [e1; e2]
    | EUpdateField (_, e1, e2, e3) -> [e1; e2; e3]
    | ENew (_, _, es) -> es
    | EPrefixOp (_, _, e) -> [e]
    | EInfixOp (_, _, e1, e2) -> [e1; e2]
    | EIf (_, e1, e2, e3) -> [e1; e2; e3]
    | EApp (_, e1, es) -> e1 :: es
    | EFunc (_, _, _, e) -> [e]
    | ELet (_, _, e1, e2) -> [e1; e2]
    | ERec (binds, e) -> e :: map thd3 binds
    | ESeq (_, e1, e2) -> [e1; e2]
    | ELabel (_, _, _, e) -> [e]
    | EBreak (_, _, e) -> [e]
    | ETryCatch (_, e1, _, e2) -> [e1; e2]
    | ETryFinally (_, e1, e2) -> [e1; e2]
    | EThrow (_, e) -> [e]
    | ETypecast (_, _, e) -> [e]
    | ERef (_, e) -> [e]
    | EDeref (_, e) -> [e]
    | ESetRef (_, e1, e2) -> [e1; e2]
    | EParens (_, e) -> [e]
    | ESubsumption (_, _, e) -> [e]

  let len = List.length

  let set_children exp children = match exp, children with
    | EConst (p, c), [] -> exp
    | EArray (p, es'), es when len es' = len es -> EArray (p, es)
    | EObject (p, ps), es when len ps = len es ->
        EObject (p, List.map2 (fun (x, _) e -> (x, e)) ps es)
    | EThis p, [] -> EThis p
    | EId (p, x), [] -> EId (p, x)
    | EBracket (p, _, _), [e1; e2] -> EBracket (p, e1, e2)
    | EUpdateField (p, _, _, _), [e1; e2; e3] -> EUpdateField (p, e1, e2, e3)
    | ENew (p, c_id, es'), es when len es' = len es -> ENew (p, c_id, es)
    | EPrefixOp (p, op, _), [e] -> EPrefixOp (p, op, e)
    | EInfixOp (p, op, _, _), [e1; e2] -> EInfixOp  (p, op, e1, e2)
    | EIf (p, _, _, _), [e1; e2; e3] -> EIf (p, e1, e2, e3)
    | EApp (p, _, args), e1 :: es when len args = len es ->
        EApp (p, e1, es)
    | EFunc (p, xs, t, _), [e] -> EFunc (p, xs, t, e)
    | ELet (p, x, _, _), [e1; e2] -> ELet (p, x, e1, e2)
    | ERec (binds, _), e :: es when len binds = len es ->
        ERec (List.map2 (fun (x, t, _) e -> (x, t, e)) binds es, e)
    | ESeq (p, _, _), [e1; e2] -> ESeq (p, e1, e2)
    | ELabel (p, l, t, _), [e] -> ELabel (p, l, t, e)
    | EBreak (p, l, _), [e] -> EBreak (p, l, e)
    | ETryCatch (p, _, x, _), [e1; e2] -> ETryCatch (p, e1, x, e2)
    | ETryFinally (p, _, _), [e1; e2] -> ETryFinally (p, e1, e2)
    | EThrow (p, _), [e] -> EThrow (p, e)
    | ETypecast (p, r, _), [e] -> ETypecast (p, r, e)
    | ERef (p, _), [e] -> ERef (p, e)
    | EDeref (p, _), [e] -> EDeref (p, e)
    | ESetRef (p, _, _), [e1; e2] -> ESetRef (p, e1, e2)
    | EParens (p, _), [e] -> EParens (p, e)
    | ESubsumption (p, t, _), [e] -> ESubsumption (p, t, e)
    | _ -> raise (Invalid_argument "Exp.set_children")

  let pos exp = match exp with
    | EConst (p, _) -> p
    | EArray (p, _) -> p
    | EObject (p, _) -> p
    | EThis p -> p
    | EId (p, _) -> p
    | EBracket (p, _, _) -> p
    | EUpdateField (p, _, _, _) -> p
    | ENew (p, _, _) -> p
    | EPrefixOp (p, _, _) -> p
    | EInfixOp (p, _, _, _) -> p
    | EIf (p, _, _, _) -> p
    | EApp (p, _, _) -> p
    | EFunc (p, _, _, _) -> p
    | ELet (p, _, _, _) -> p
    | ERec (_, _) -> failwith "Exp.pos of ERec"
    | ESeq (p, _, _) -> p
    | ELabel (p, _, _, _) -> p
    | EBreak (p, _, _) -> p
    | ETryCatch (p, _, _, _) -> p
    | ETryFinally (p, _, _) -> p
    | EThrow (p, _) -> p
    | ETypecast (p, _, _) -> p
    | ERef (p, _) -> p
    | EDeref (p, _) -> p
    | ESetRef (p, _, _) -> p
    | EParens (p, _) -> p
    | ESubsumption (p, _, _) -> p

end

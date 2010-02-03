open Prelude

type runtime_typ =
    RTNumber
  | RTString
  | RTBoolean
  | RTFunction
  | RTObject
  | RTUndefined

module RTOrdered = struct
  type t = runtime_typ
  let compare = Pervasives.compare
end

module RTSet = Set.Make (RTOrdered)

module RTSetExt = SetExt.Make (RTSet)

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
  | EBreak of 'a * id * 'a exp
  | ETryCatch of 'a * 'a exp * id * 'a exp
  | ETryFinally of 'a * 'a exp * 'a exp
  | EThrow of 'a * 'a exp
  | ETypecast of 'a * runtime_typs * 'a exp

and 'a lvalue =
    LVar of 'a * id
  | LProp of 'a * 'a exp * 'a exp

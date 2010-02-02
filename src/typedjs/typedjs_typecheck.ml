open Prelude
open Typedjs_syntax
open Typedjs_types


let rec tc env exp = match exp with
    EString _ -> typ_string
  | ERegexp _ -> typ_regexp
  | ENum _ -> typ_num
  | EInt -> typ_int
  | EBool _ -> typ_bool
  | ENull _ -> typ_null
(*
  | EArray of 'a * 'a exp list
  | EObject of 'a * (string * 'a exp) list
  | EThis of 'a 
*)
  | EId (p, x) ->
      try IdMap.find x env with
        Not_found -> failwith (sprintf "%s is unbound" x)

(*
  | EBracket of 'a * 'a exp * 'a exp
  | ENew of 'a * 'a exp * 'a exp list
  | EPrefixOp of 'a * JavaScript_syntax.prefixOp * 'a exp
  | EInfixOp of 'a * JavaScript_syntax.infixOp * 'a exp * 'a exp
*)
  | EIf of 'a * 'a exp * 'a exp * 'a exp
  | EAssign of 'a * 'a lvalue * 'a exp
  | EApp of 'a * 'a exp * 'a exp list
  | EFunc of 'a * id list * typ * 'a exp
  | EUndefined of 'a
  | ELet of 'a * (id * 'a exp) list * 'a exp
  | ERec of (id * 'a exp) list * 'a exp
  | ESeq of 'a * 'a exp * 'a exp
  | EWhile of 'a * 'a exp * 'a exp
  | ELabel of 'a * id * 'a exp
  | EBreak of 'a * id * 'a exp
  | ETryCatch of 'a * 'a exp * id * 'a exp
  | ETryFinally of 'a * 'a exp * 'a exp
  | EThrow of 'a * 'a exp

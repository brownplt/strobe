open Prelude

type op1 = Op1Prefix of JavaScript_syntax.prefixOp

type op2 = 
  | Op2Infix of JavaScript_syntax.infixOp
  | GetField
  | DeleteField
  | SetRef

type exp =
    EConst of pos * Exprjs_syntax.const
  | EId of pos * id
  | EArray of pos * exp list
  | EObject of pos * (pos * string * exp) list
  | EUpdateField of pos * exp * exp * exp
  | EOp1 of pos * op1 * exp
  | EOp2 of pos * op2 * exp * exp
  | EIf of pos * exp * exp * exp
  | EApp of pos * exp * exp list
  | ERef of pos * exp
  | EDeref of pos * exp
  | ESeq of pos * exp * exp
  | ELet of pos * id * exp * exp
  | EFix of pos * (id * exp) list * exp 
      (** All bindings must be [ELambda]s. *)
  | ELabel of pos * id * exp
  | EBreak of pos * id * exp
  | ETryCatch of pos * exp * exp
      (** Catch block must be an [ELambda] *)
  | ETryFinally of pos * exp * exp
  | EThrow of pos * exp
  | ELambda of pos * id list * exp

(******************************************************************************)

open Exprjs_syntax

type env = IdSet.t
 
let rec ds_expr (env : env) (expr : expr) : exp = match expr with
    ConstExpr (p, c) -> EConst (p, c)
  | ArrayExpr (p, es) -> EArray (p, map (ds_expr env) es)
  | ObjectExpr (p, fields) -> 
      (* Imperative object *)
      ERef (p, EObject (p, map (ds_field env) fields))
  | ThisExpr p -> 
      (* In JavaScript, 'this' is a reserved word.  Hence, we are certain that
         the the bound identifier is not captured by existing bindings. *)
      EId (p, "this")
  | VarExpr (p, x) -> 
      if IdSet.mem x env then
        (* var-lifting would have introduced a binding for x. *)
        EId (p, x)
      else
        EOp2 (p, GetField, EDeref (p, EId (p, "#global")),
              EConst (p, CString x))
  | BracketExpr (p, e1, e2) ->
      EOp2 (p, GetField, EDeref (p, ds_expr env e1), ds_expr env e2)
(*   | NewExpr of pos * expr * expr list *)
  | PrefixExpr (p, op, e) -> EOp1 (p, Op1Prefix op, ds_expr env e)
  | InfixExpr (p, op, e1, e2) -> EOp2 (p, Op2Infix op, ds_expr env e1, ds_expr env e2)
  | IfExpr (p, e1, e2, e3) -> 
      EIf (p, ds_expr env e1, ds_expr env e2, ds_expr env e3)
  | AssignExpr (p, VarLValue (p', x), e) -> 
      if IdSet.mem x env then
        EOp2 (p, SetRef, EDeref (p', EId (p, x)), ds_expr env e)
      else
        EOp2 (p, SetRef, EId (p, "#global"),
              EUpdateField (p, (EDeref (p, EId (p, "#global"))),
                            EConst (p, CString x),
                            ds_expr env e))
  | AssignExpr (p, PropLValue (p', e1, e2), e3) -> 
      ELet (p, "%obj", ds_expr env e1,
            EOp2 (p, SetRef, EId (p, "%obj"), 
                  EUpdateField (p, EDeref (p, EId (p, "%obj")),
                                ds_expr env e1,
                                ds_expr env e2)))
  | LetExpr (p, x, e1, e2) ->
      ELet (p, x, ds_expr env e1, ds_expr (IdSet.add x env) e2)
  | SeqExpr (p, e1, e2) -> 
      ESeq (p, ds_expr env e1, ds_expr env e2)
  | WhileExpr (p, test, body) -> 
      EFix (p, 
            [ ("%while", 
               ELambda (p, [], 
                        EIf (p, ds_expr env test, 
                             ESeq (p, ds_expr env body, 
                                   EApp (p, EId (p, "%while"), [])),
                             EConst (p, CUndefined)))) ],
            EApp (p, EId (p, "%while"), []))
  | DoWhileExpr (p, body, test) -> 
      EFix (p, 
            [ ("%dowhile", 
               ELambda (p, [], 
                        ESeq (p, ds_expr env body,
                              EIf (p, ds_expr env test, 
                                   EApp (p, EId (p, "%while"), []),
                                   EConst (p, CUndefined))))) ],
            EApp (p, EId (p, "%dowhile"), []))
  | LabelledExpr (p, l, e) ->
      ELabel (p, l, ds_expr env e)
  | BreakExpr (p, l, e) -> EBreak (p, l, ds_expr env e)
  | VarDeclExpr (p, x, e) -> 
      (* var-lifting would have introduced a binding for x. *)
      EOp2 (p, SetRef, EId (p, x), ds_expr env e)
  | TryCatchExpr (p, body, x, catch) ->
      ETryCatch (p, ds_expr env body, ELambda (p, [x], ds_expr env catch))
  | TryFinallyExpr (p, e1, e2) -> 
      ETryFinally (p, ds_expr env e1, ds_expr env e2)
  | ThrowExpr (p, e) -> EThrow (p, ds_expr env e)
  | AppExpr (p, BracketExpr (p', obj, prop), args) ->
      ELet (p, "%obj", ds_expr env obj,
            EApp (p, EOp2 (p', GetField, EDeref (p', EId (p, "%obj")),
                                ds_expr env prop),
                  [ EId (p, "%obj"); 
                    EArray (p, map (ds_expr env) args) ]))
  | AppExpr (p, f, args) ->
      EApp (p, ds_expr env f,
            [ EId (p, "#global"); 
              EArray (p, map (ds_expr env) args) ])
  | FuncExpr (p, args, body) ->
      let init_var x exp =
        ELet (p, x, ERef (p, EConst (p, CUndefined)), exp)
      and get_arg x n exp =
        ELet (p, x, EOp2 (p, GetField, EDeref (p, EId (p, "arguments")),
                               EConst (p, CString (string_of_int n))),
              exp) 
      and vars = Exprjs_syntax.locals body in
        ELambda 
          (p, [ "this"; "arguments"],
           List.fold_right2 get_arg args (iota (List.length args))
             (fold_right init_var (IdSetExt.to_list vars)
                (ds_expr (IdSet.union vars env) body)))

and ds_field env (p, x, e) = (p, x, ds_expr env e)

let desugar (expr : expr) = 
  ds_expr IdSet.empty expr

(******************************************************************************)

open Format
open FormatExt

let p_op1 op = match op with
    Op1Prefix o -> text (JavaScript_pretty.render_prefixOp o)

let p_op2 op = match op with
  | Op2Infix o -> text (JavaScript_pretty.render_infixOp o)
  | GetField -> text "get-field"
  | DeleteField -> text "delete-field"
  | SetRef -> text "set-ref"



    

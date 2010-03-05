open Prelude

type op1 = 
  | Op1Prefix of JavaScript_syntax.prefixOp
  | Deref
  | Ref

type op2 = 
  | Op2Infix of JavaScript_syntax.infixOp
  | Prim2 of string
  | GetField
  | DeleteField
  | SetRef

type exp =
    EConst of pos * Exprjs_syntax.const
  | EId of pos * id
  | EObject of pos * (pos * string * exp) list
  | EUpdateField of pos * exp * exp * exp
  | EOp1 of pos * op1 * exp
  | EOp2 of pos * op2 * exp * exp
  | EIf of pos * exp * exp * exp
  | EApp of pos * exp * exp list
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

type env = bool IdMap.t

let rec mk_array (p, exps) = 
  let mk_field n v = (p, string_of_int n, v) in
    EObject (p, List.map2 mk_field (iota (List.length exps)) exps)
 
let rec ds_expr (env : env) (expr : expr) : exp = match expr with
    ConstExpr (p, c) -> EConst (p, c)
  | ArrayExpr (p, es) -> mk_array (p, map (ds_expr env) es)
  | ObjectExpr (p, fields) -> 
      (* Imperative object *)
      EOp1 (p, Ref, EObject (p, map (ds_field env) fields))
  | ThisExpr p -> 
      (* In JavaScript, 'this' is a reserved word.  Hence, we are certain that
         the the bound identifier is not captured by existing bindings. *)
      EId (p, "this")
  | VarExpr (p, x) -> begin
      try 
        if IdMap.find x env then
          (* var-lifting would have introduced a binding for x. *)
          EOp1 (p, Deref, EId (p, x))
        else
          EId (p, x)
      with Not_found ->
        EOp2 (p, GetField, EOp1 (p, Deref, EId (p, "#global")),
              EConst (p, CString x))
    end
  | BracketExpr (p, e1, e2) ->
      EOp2 (p, GetField, EOp1 (p, Deref, ds_expr env e1), ds_expr env e2)
  | PrefixExpr (p, op, e) -> EOp1 (p, Op1Prefix op, ds_expr env e)
  | InfixExpr (p, op, e1, e2) ->
      EOp2 (p, Op2Infix op, ds_expr env e1, ds_expr env e2)
  | IfExpr (p, e1, e2, e3) -> 
      EIf (p, ds_expr env e1, ds_expr env e2, ds_expr env e3)
  | AssignExpr (p, VarLValue (p', x), e) -> 
      if IdMap.mem x env then (* assume var-bound *)
        EOp2 (p, SetRef, EOp1 (p', Deref, EId (p, x)), ds_expr env e)
      else
        EOp2 (p, SetRef, EId (p, "#global"),
              EUpdateField (p, (EOp1 (p, Deref, EId (p, "#global"))),
                            EConst (p, CString x),
                            ds_expr env e))
  | AssignExpr (p, PropLValue (p', e1, e2), e3) -> 
      ELet (p, "%obj", ds_expr env e1,
            EOp2 (p, SetRef, EId (p, "%obj"), 
                  EUpdateField (p, EOp1 (p, Deref, EId (p, "%obj")),
                                ds_expr env e2,
                                ds_expr env e3)))
  | LetExpr (p, x, e1, e2) ->
      ELet (p, x, ds_expr env e1, ds_expr (IdMap.add x false env) e2)
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
      if IdMap.mem x env then
        (* var-lifting would have introduced a binding for x. *)
        EOp2 (p, SetRef, EId (p, x), ds_expr env e)
      else 
        EOp2 (p, SetRef, EId (p, "#global"),
              EUpdateField (p, EOp1 (p, Deref, EId (p, "#global")),
                            EConst (p, CString x),
                            ds_expr env e))
  | TryCatchExpr (p, body, x, catch) ->
      ETryCatch (p, ds_expr env body, ELambda (p, [x], ds_expr env catch))
  | TryFinallyExpr (p, e1, e2) -> 
      ETryFinally (p, ds_expr env e1, ds_expr env e2)
  | ThrowExpr (p, e) -> EThrow (p, ds_expr env e)
  | AppExpr (p, BracketExpr (p', obj, prop), args) ->
      ELet (p, "%obj", ds_expr env obj,
            EApp (p, EOp2 (p', GetField, EOp1 (p', Deref, EId (p, "%obj")),
                                ds_expr env prop),
                  [ EId (p, "%obj"); 
                    mk_array (p, map (ds_expr env) args) ]))
  | AppExpr (p, f, args) ->
      EApp (p, ds_expr env f,
            [ EId (p, "#global"); 
              EOp1 (p, Ref, mk_array (p, map (ds_expr env) args)) ])
  | NewExpr (p, constr, args) -> (* TODO: FIX THIS AND APP *)
      ELet (p, "%constr", ds_expr env constr,
            EApp (p, EId (p, "%constr"),
                  [ EObject (p, [ (p, "__proto__", 
                                   EOp2 (p, GetField,
                                         EOp1 (p, Deref, EId (p, "%constr")),
                                         EConst (p, CString "prototype"))) ]);
                    EOp1 (p, Ref, mk_array (p, map (ds_expr env) args)) ]))

  | FuncExpr (p, args, body) ->
      let init_var x exp =
        ELet (p, x, EOp1 (p, Ref, EConst (p, CUndefined)), exp)
      and get_arg x n exp =
        ELet (p, x, 
              EOp1 (p, Ref,
                    EOp2 (p, GetField, EOp1 (p, Deref, EId (p, "arguments")),
                          EConst (p, CString (string_of_int n)))),
              exp) 
      and vars = Exprjs_syntax.locals body in
      let env = IdSet.fold (fun x env -> IdMap.add x true env) vars env in
      let env = fold_left (fun env x -> IdMap.add x true env) env args in
      let env = IdMap.add "arguments" false (IdMap.add "this" false env) in
        ELambda 
          (p, [ "this"; "arguments"],
           List.fold_right2 get_arg args (iota (List.length args))
             (fold_right init_var (IdSetExt.to_list vars)
                (ds_expr env body)))
  | FuncStmtExpr (p, f, args, body) ->
      EOp2 (p, SetRef, EId (p, f), ds_expr env (FuncExpr (p, args, body)))


and ds_field env (p, x, e) = (p, x, ds_expr env e)


let p = (Lexing.dummy_pos, Lexing.dummy_pos)

let desugar (expr : expr) = 
  ELet (p, "#global", EOp1 (p, Ref, EObject (p, [])),
        ELet (p, "%uncaught-exception", EObject (p, []),
              ELet (p, "%return-value", EObject (p, []),
                    ds_expr IdMap.empty expr)))

(******************************************************************************)

open Format
open FormatExt

let p_op1 op = match op with
  | Op1Prefix o -> text (JavaScript_pretty.render_prefixOp o)
  | Deref -> text "deref"
  | Ref -> text "ref"

let p_op2 op = match op with
  | Op2Infix o -> text (JavaScript_pretty.render_infixOp o)
  | GetField -> text "get-field"
  | DeleteField -> text "delete-field"
  | SetRef -> text "set-ref"
  | Prim2 s -> text s



    

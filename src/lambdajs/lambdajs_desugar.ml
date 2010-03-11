open Prelude
open Lambdajs_syntax
open Exprjs_syntax (* for constants *)

module DesugarOp = struct

  open JavaScript_syntax (* for infixOp and prefixOp *)

  let rec numnum p op e1 e2 = 
    EOp2 (p, Prim2 op, 
          EApp (p, EId (p, "[[toNumber]]"), [ desugar e1 ]),
          EApp (p, EId (p, "[[toNumber]]"), [ desugar e2 ]))

  and desugar (exp : exp) = match exp with
    | EIf (p, e1, e2, e3) ->
        EIf (p, EOp1 (p, Prim1 "prim->bool", desugar e1), 
             desugar e2, desugar e3)
    | EOp1 (p, Op1Prefix op, e) -> begin match op with
        | PrefixLNot -> 
            EIf (p, EOp1 (p, Prim1 "prim->bool", desugar e),
                 EConst (p, CBool false),
                 EConst (p, CBool true))
        | PrefixBNot ->
            EOp1 (p, Prim1 "~",
                  EOp1 (p, Prim1 "to-int-32",
                        EApp (p, EId (p, "[[toNumber]]"), [ desugar e ])))
        | PrefixPlus ->
            EApp (p, EId (p, "[[toNumber]]"), [ desugar e ])
        | PrefixMinus ->
            EOp2 (p, Prim2 "-", 
                  EConst (p, CNum 0.0),
                  EApp (p, EId (p, "[[toNumber]]"), [ desugar e ]))
        | PrefixTypeof ->
            EOp1 (p, Prim1 "surface-typeof", 
                  EApp (p, EId (p, "[[getValue]]"), [ desugar e ]))
        | PrefixVoid ->
            ESeq (p, e, EConst (p, CUndefined))
        | PrefixDelete -> begin match e with
            | EOp2 (p', GetField, EOp1 (p'', Deref, obj), field) ->
                EApp (p, EId (p, "[[safeDelete]]"), 
                      [ desugar obj; desugar field ])
            | _ -> 
                ESeq (p, desugar e, EConst (p, CBool true))
          end
      end
    | EOp2 (p, UnsafeGetField, e1, e2) ->
        EOp2 (p, GetField, 
              EApp (p, EId (p, "[[toObject]]"), [ e1 ]),
              EApp (p, EId (p, "[[toString]]"), [ e2 ]))
    | EOp2 (p, Op2Infix op, e1, e2) -> begin match op with
          (* TODO: check comments in Claudiu's implementation *)
        | OpLT -> numnum p "<" e1 e2
        | OpLEq -> numnum p "<=" e1 e2
        | OpGT -> numnum p ">" e1 e2
        | OpGEq -> numnum p ">=" e1 e2
        | OpDiv -> numnum p "/" e1 e2
        | OpMul -> numnum p "*" e1 e2
        | OpMod -> numnum p "%" e1 e2
        | OpSub -> numnum p "-" e1 e2
        | OpLShift -> numnum p "<<" e1 e2
        | OpSpRShift -> numnum p ">>" e1 e2
        | OpZfRShift -> numnum p ">>>" e1 e2
        | OpBAnd -> numnum p "&" e1 e2
        | OpBXor -> numnum p "^" e1 e2
        | OpBOr -> numnum p "|" e1 e2
        | OpLAnd ->
            (* NOTE: If e1 is false-valued, we return the value of e1, which
               may be false, or zero, or ... *)               
            ELet (p, "[[and-lhs]]", desugar e1,
                  EIf (p, EOp1 (p, Prim1 "prim->bool", EId (p, "[[and-lhs]]")),
                       desugar e2, 
                       EId (p, "[[and-lhs]]")))
        | OpLOr ->
            ELet (p, "[[or-lhs]]", desugar e1,
                  EIf (p, EOp1 (p, Prim1 "prim->bool", EId (p, "[[or-lhs]]")),
                       EId (p, "[[or-lhs]]"),
                       desugar e2))
        | OpEq ->
            EApp (p, EId (p, "[[abstractEquality]]"),
                  [ desugar e1; desugar e2 ])
        | OpNEq ->
            EIf (p, EApp (p, EId (p, "[[abstractEquality]]"), 
                          [ desugar e1; desugar e2 ] ),
                 EConst (p, CBool false),
                 EConst (p, CBool true))
        | OpStrictEq ->
            EOp2 (p, Prim2 "stx=", desugar e1, desugar e2)
        | OpStrictNEq ->
            EIf (p, EOp2 (p, Prim2 "stx=", desugar e1, desugar e2),
                 EConst (p, CBool false),
                 EConst (p, CBool true))
        | OpIn ->
            EApp (p, EId (p, "[[inOperator]]"),
                  [ EApp (p, EId (p, "[[toString]]"), [ desugar e1 ]);
                    EApp (p, EId (p, "[[toObject]]"), [ desugar e2 ]) ])
        | OpInstanceof ->
            EApp (p, EId (p, "[[instanceofOperator]]"),
                  [ desugar e1; desugar e2 ])
        | OpAdd ->
            EOp2 (p, Prim2 "+",
                  EApp (p, EId (p, "[[toPrimitive]]"), [ desugar e1 ]),
                  EApp (p, EId (p, "[[toPrimitive]]"), [ desugar e2 ]))
      end
  | EConst _ -> exp
  | EId _ -> exp
  | EObject (p, fields) -> EObject (p, map (third3 desugar) fields)
  | EUpdateField (p, e1, e2, e3) ->
      EUpdateField (p, desugar e1, desugar e2, desugar e3)
  | EOp1 (p, op, e) -> EOp1 (p, op, desugar e)
  | EOp2 (p, op, e1, e2) -> EOp2 (p, op, desugar e1, desugar e2)
  | EApp (p, f, args) -> EApp (p, desugar f, map desugar args)
  | ESeq (p, e1, e2) -> ESeq (p, desugar e1, desugar e2)
  | ELet (p, x, e1, e2) -> ELet (p, x, desugar e1, desugar e2)
  | EFix (p, binds, body) -> EFix (p, map (second2 desugar) binds, desugar body)
  | ELabel (p, l, e) -> ELabel (p, l, desugar e)
  | EBreak (p, l, e) -> EBreak (p, l, desugar e)
  | ETryCatch (p, e1, e2) -> ETryCatch (p, desugar e1, desugar e2)
  | ETryFinally (p, e1, e2) -> ETryFinally (p, desugar e1, desugar e2)
  | EThrow (p, e) -> EThrow (p, desugar e)
  | ELambda (p, args, body) -> ELambda (p, args, desugar body)

end (* struct *)

let desugar_op = DesugarOp.desugar

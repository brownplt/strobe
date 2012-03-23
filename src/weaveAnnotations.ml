open Prelude
open Typedjs_syntax
open Sb_desugar
open ReadTyps

let rec weave_rec (db : typ_db) (exp : exp) = 
  let w = weave db in
  match exp with
  | EConst _ -> exp
  | EBot _ -> exp
  | EAssertTyp (p, t, e) -> EAssertTyp (p, t, w e)
  | EArray (p, es) -> EArray (p, map w es)
  | EObject (p, props) -> EObject (p, map (second2 w) props)
  | EId _ -> exp
  | EBracket (p, e1, e2) -> EBracket (p, w e1, w e2)
  | EUpdate (p, e1, e2, e3) -> EUpdate (p, w e1, w e2, w e3)
  | EPrefixOp (p, op, e) -> EPrefixOp (p, op, w e)
  | EInfixOp (p, op, e1, e2) -> EInfixOp (p, op, w e1, w e2)
  | EIf (p, e1, e2, e3) -> EIf (p, w e1, w e2, w e3)
  | EApp (p, e, es) -> EApp (p, w e, map w es)
  | EFunc (p, xs, info, e) -> EFunc (p, xs, info, w e)
  | ELet (p, x, e1, e2) -> 
    let e1' = w e1 in
    let e2' = w e2 in
    ELet (p, x, e1', e2')
  | ERec (p, binds, e) -> ERec (p, map (third3 w) binds, w e)
  | ESeq (p, e1, e2) -> ESeq (p, w e1, w e2)
  | ELabel (p, x, e) -> ELabel (p, x, w e)
  | EBreak (p, x, e) -> EBreak (p, x, w e)
  | ETryCatch (p, e1, x, e2) -> ETryCatch (p, w e1, x, w e2)
  | ETryFinally (p, e1, e2) -> ETryFinally (p, w e1, w e2)
  | EThrow (p, e) -> EThrow (p, w e)
  | ETypecast (p, rt, e) -> ETypecast (p, rt, w e)
  | ERef (p, k, e) -> ERef (p, k, w e)
  | EDeref (p, e) -> EDeref (p, w e)
  | ESetRef (p, e1, e2) -> ESetRef (p, w e1, w e2)
  | EParen (p, e) -> EParen (p, w e)
  | ESubsumption (p, t, e) -> ESubsumption (p, t, w e)
  | EDowncast (p, t, e) -> EDowncast (p, t, w e)
  | ETypAbs (p, x, t, e) -> ETypAbs (p, x, t, w e)
  | ETypApp (p, e, t) -> ETypApp (p, w e, t)
  | ECheat (p, t, e) -> ECheat (p, t, w e)

and weave (db : typ_db) (exp : exp) = match exp with
  | ERec _ -> weave_rec db exp
  | _ -> 
    let p = Exp.pos exp in
    match get_annotation db p with
      | None -> weave_rec db exp
      | Some (ATyp wt) ->
        (* TODO(arjun): p is not the position of the annotation!!! *)
        EAssertTyp (p, desugar_typ p wt, weave_rec db exp)
      | Some (ACheat wt) -> ECheat (p, desugar_typ p wt, exp)
      | Some (AUpcast wt) -> 
          ESubsumption (p, desugar_typ p wt, weave_rec db exp)
      | Some (ADowncast wt) -> 
          EDowncast (p, desugar_typ p wt, weave_rec db exp)
      | Some (ATypAbs (x, wt)) -> 
          ETypAbs (p, x, desugar_typ p wt, weave_rec db exp)
      | Some (ATypApp wt) -> 
          ETypApp (p, weave_rec db exp, desugar_typ p wt)
      | Some (AAssertTyp wt) -> 
          EAssertTyp (p, desugar_typ p wt, weave_rec db exp)

let rec assert_typ typ exp = 
  let w e = assert_typ typ e in match exp with
    | ESetRef (p, e1, ERef(_, _, EAssertTyp _)) -> exp
    | ESetRef (p, e1, ERef(pr, pk, e)) -> ESetRef(p, e1, EAssertTyp(pr, typ, ERef(pr, pk, e)))
    | ESetRef (p, e1, e2) -> ESetRef(p, e1, EAssertTyp(p, typ, e2))
    | EConst _
    | EBot _
    | EAssertTyp _ -> exp
    | EArray (p, es) -> EArray (p, map w es)
    | EObject (p, props) -> EObject (p, map (second2 w) props)
    | EId _ -> exp
    | EBracket (p, e1, e2) -> EBracket (p, w e1, w e2)
    | EUpdate (p, e1, e2, e3) -> EUpdate (p, w e1, w e2, w e3)
    | EPrefixOp (p, op, e) -> EPrefixOp (p, op, w e)
    | EInfixOp (p, op, e1, e2) -> EInfixOp (p, op, w e1, w e2)
    | EIf (p, e1, e2, e3) -> EIf (p, w e1, w e2, w e3)
    | EApp (p, e, es) -> EApp (p, w e, map w es)
    | EFunc (p, xs, info, e) -> EFunc (p, xs, info, w e)
    | ELet (p, x, e1, e2) -> 
      let e1' = w e1 in
      let e2' = w e2 in
      ELet (p, x, e1', e2')
    | ERec (p, binds, e) -> ERec (p, map (third3 w) binds, w e)
    | ESeq (p, e1, e2) -> ESeq (p, w e1, w e2)
    | ELabel (p, x, e) -> ELabel (p, x, w e)
    | EBreak (p, x, e) -> EBreak (p, x, w e)
    | ETryCatch (p, e1, x, e2) -> ETryCatch (p, w e1, x, w e2)
    | ETryFinally (p, e1, e2) -> ETryFinally (p, w e1, w e2)
    | EThrow (p, e) -> EThrow (p, w e)
    | ETypecast (p, rt, e) -> ETypecast (p, rt, w e)
    | ERef (p, k, e) -> ERef (p, k, w e)
    | EDeref (p, e) -> EDeref (p, w e)
    | EParen (p, e) -> EParen (p, w e)
    | ESubsumption (p, t, e) -> ESubsumption (p, t, w e)
    | EDowncast (p, t, e) -> EDowncast (p, t, w e)
    | ETypAbs (p, x, t, e) -> ETypAbs (p, x, t, w e)
    | ETypApp (p, e, t) -> ETypApp (p, w e, t)
    | ECheat (p, t, e) -> ECheat (p, t, w e)
      

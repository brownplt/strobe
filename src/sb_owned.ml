open Prelude
open Typedjs_syntax

let rec fold_map (f : 'c -> 'a -> 'c * 'b) (lst : 'a list) 
    (acc : 'c) : 'c * 'b list = match lst with
      | [] -> (acc, [])
      | a :: rest ->
        let (acc, rest') = fold_map f rest acc in
        let (acc, b) = f acc a in
        (acc, b :: rest')

(* Calculation of owned identifiers, assuming all bound identifiers are unique:

   1. Accumulate the set of identifiers x, that appear in local
   [ESetRef (_, EId (_, x), _)] expressions, and store this set in the
   enclosing [func_info]. The resulting sets will overlap.
   
   2. Fold over these sets, accumulating a set of all assigned variables, [acc].
   Rewrite [func_info.func_owned] to [IdSet.diff func_info.func_owned acc].
   The resulting sets are thus mutually disjoint.

   Integrate the two steps with two accumulators:

   1. The set of locally assigned identifiers, and

   2. The set of all assigned identifiers. *)

let rec f (acc : IdSet.t * IdSet.t) (exp : exp) : (IdSet.t * IdSet.t) * exp =
  match exp with
  | EConst _ -> (acc, exp)
  | EBot _ -> (acc, exp)
  | EAssertTyp (p, t, e) -> 
    let (acc, e') = f acc e in
    (acc, EAssertTyp (p, t, e'))
  | EArray (p, es) ->
    let (acc', es) = fold_map f es acc in
    (acc', EArray (p, es))
  | EEmptyArray _ -> (acc, exp)
  | EObject (p, fs) ->
    let (acc', fs') = fold_map 
      (fun acc (s, e) ->  let (acc, e) = f acc e in (acc, (s, e)))
      fs acc in
    (acc, EObject (p, fs'))
  | EId _ -> (acc, exp)
  | EBracket (p, e1, e2) ->
    let (acc, e1') = f acc e1 in
    let (acc, e2') = f acc e2 in
    (acc, EBracket (p, e1', e2'))
  | EUpdate (p, e1, e2, e3) ->
    let (acc, e1) = f acc e1 in
    let (acc, e2) = f acc e2 in
    let (acc, e3) = f acc e3 in
    (acc, EUpdate (p, e1, e2, e3))
  | EPrefixOp (p, x, e) ->
    let (acc, e) = f acc e in
    (acc, EPrefixOp (p, x, e))
  | EInfixOp (p, x, e1, e2) ->
    let (acc, e1) = f acc e1 in
    let (acc, e2) = f acc e2 in
    (acc, EInfixOp (p, x, e1, e2))
  | EIf (p, e1, e2, e3) ->
    let (acc, e1) = f acc e1 in
    let (acc, e2) = f acc e2 in
    let (acc, e3) = f acc e3 in
    (acc, EIf (p, e1, e2, e3))
  | EApp (p, e, es) ->
    let (acc, e) = f acc e in
    let (acc, es) = fold_map f es acc in
    (acc, EApp (p, e, es))
  | EFunc (p, xs, fi, e) ->
    let (all1, enclosing_local) = acc in
    let ((all2, internal_local), e) = f (all1, IdSet.empty) e in
    ((all2, enclosing_local),
     EFunc (p, xs, { fi with func_owned = IdSet.diff internal_local all1 }, e))
  | ELet (p, x, e1, e2) ->
    let (acc, e1) = f acc e1 in
    let (acc, e2) = f acc e2 in
    (acc, ELet (p, x, e1, e2))
  | ERec (binds, body) ->
    let (acc, binds) = fold_map
      (fun acc (x, t, e) -> let (acc, e) = f acc e in (acc, (x, t, e)))
      binds acc in
    let (acc, body) = f acc body in
    (acc, ERec (binds, body))
  | ESeq (p, e1, e2) ->
    let (acc, e1) = f acc e1 in
    let (acc, e2) = f acc e2 in
    (acc, ESeq (p, e1, e2))
  | ELabel (p, x, e) ->
    let (acc, e) = f acc e in
    (acc, ELabel (p, x, e))
  | EBreak (p, x, e) ->
    let (acc, e) = f acc e in
    (acc, EBreak (p, x, e))
  | ETryCatch (p, e1, x, e2) ->
    let (acc, e1) = f acc e1 in
    let (acc, e2) = f acc e2 in
    (acc, ETryCatch (p, e1, x, e2))
  | ETryFinally (p, e1, e2) ->
    let (acc, e1) = f acc e1 in
    let (acc, e2) = f acc e2 in
    (acc, ETryFinally (p, e1, e2))
  | EThrow (p, e) ->
    let (acc, e) = f acc e in
    (acc, EThrow (p, e))
  | ETypecast (p, rt, e) ->
    let (acc, e) = f acc e in
    (acc, ETypecast (p, rt, e))
  | ERef (p, k, e) ->
    let (acc, e) = f acc e in
    (acc, ERef (p, k, e))
  | EDeref (p, e) ->
    let (acc, e) = f acc e in
    (acc, EDeref (p, e))
  | ESetRef (p, EId (p2, x), e) ->
    let (all, locals) = acc in
    let (all, locals) = IdSet.add x all, IdSet.add x locals in
    let (acc, e) = f (all, locals) e in
    (acc, ESetRef (p, EId (p2, x), e))
  | ESetRef (p, e1, e2) ->
    let (acc, e1) = f acc e1 in
    let (acc, e2) = f acc e2 in
    (acc, ESetRef (p, e1, e2))
  | ESubsumption (p, t, e) ->
    let (acc, e) = f acc e in
    (acc, ESubsumption (p, t, e))
  | EDowncast (p, t, e) ->
    let (acc, e) = f acc e in
    (acc, EDowncast (p, t, e))
  | ETypAbs (p, x, t, e) ->
    let (acc, e) = f acc e in
    (acc, ETypAbs (p, x, t, e))
  | ETypApp (p, e, t) ->
    let (acc, e) = f acc e in
    (acc, ETypApp (p, e, t))
  | ECheat (p, t, e) ->
    let (acc, e) = f acc e in
    (acc, ECheat (p, t, e))

let  owned_inference (prog : exp) = 
  let (_, prog) = f (IdSet.empty, IdSet.empty) prog in
  prog

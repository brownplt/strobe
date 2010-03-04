open Prelude
open Typedjs_syntax

open IdSet
open IdSetExt


let rec local_av_exp (exp : exp) : IdSet.t = match exp with
    EConst _ -> IdSet.empty
  | EArray (_, es) -> IdSetExt.unions (map local_av_exp es)
  | EObject (_, ps) -> 
      IdSetExt.unions (map (fun (_, _, e) -> local_av_exp e) ps)
  | EThis _ -> IdSet.empty
  | EId _ -> IdSet.empty
  | EBracket (_, e1, e2) -> IdSet.union (local_av_exp e1) (local_av_exp e2)
  | ENew (_, c, args) -> IdSetExt.unions (map local_av_exp (c :: args))
  | EPrefixOp (_, _, e) -> local_av_exp e
  | EInfixOp (_, _, e1, e2) -> IdSet.union (local_av_exp e1) (local_av_exp e2)
  | EIf (_, e1, e2, e3) -> IdSetExt.unions (map local_av_exp [e1; e2; e3])
  | EApp (_, f, args) -> IdSetExt.unions (map local_av_exp (f :: args))
  | EFunc (_, args, _, e) -> IdSet.empty (* do not recur into functions *)
  | ELet (_, x, e1, e2) ->
      IdSet.union (local_av_exp e1) (IdSet.remove x (local_av_exp e2))
  | ERec (binds, e) ->
      IdSet.diff (IdSetExt.unions (map local_av_exp (e :: map thd3 binds)))
        (IdSetExt.from_list (map fst3 binds))
  | ESeq (_, e1, e2) -> IdSet.union (local_av_exp e1) (local_av_exp e2)
  | ELabel (_, _, _, e) -> local_av_exp e
  | EBreak (_, _, e) -> local_av_exp e
  | ETryCatch (_, e1, x, e2) -> 
      IdSet.union (local_av_exp e1) (IdSet.remove x (local_av_exp e2))
  | ETryFinally (_, e1, e2) -> IdSet.union (local_av_exp e1) (local_av_exp e2)
  | EThrow (_, e) -> local_av_exp e
  | ETypecast (_, _, e) -> local_av_exp e

let rec local_av_def (def : def) : IdSet.t = match def with
    DEnd -> IdSet.empty
  | DExp (e, d) -> IdSet.union (local_av_exp e) (local_av_def d)
  | DLet (_, x, e, d) ->
      IdSet.union (local_av_exp e) (IdSet.remove x (local_av_def d))
  | DRec (binds, d) ->
      IdSet.diff 
        (IdSetExt.unions 
           (local_av_def d :: (map local_av_exp (map thd3 binds))))
        (IdSetExt.from_list (map fst3 binds))

let rec av_exp (exp : exp) : IdSet.t = match exp with
    EConst _ -> IdSet.empty
  | EArray (_, es) -> IdSetExt.unions (map av_exp es)
  | EObject (_, ps) -> IdSetExt.unions (map (fun (_, _, e) -> av_exp e) ps)
  | EThis _ -> IdSet.empty
  | EId _ -> IdSet.empty
  | EBracket (_, e1, e2) -> IdSet.union (av_exp e1) (av_exp e2)
  | ENew (_, c, args) -> IdSetExt.unions (map av_exp (c :: args))
  | EPrefixOp (_, _, e) -> av_exp e
  | EInfixOp (_, _, e1, e2) -> IdSet.union (av_exp e1) (av_exp e2)
  | EIf (_, e1, e2, e3) -> IdSetExt.unions (map av_exp [e1; e2; e3])
  | EApp (_, f, args) -> IdSetExt.unions (map av_exp (f :: args))
  | EFunc (_, args, _, e) -> IdSet.diff (av_exp e) (IdSetExt.from_list args)
  | ELet (_, x, e1, e2) -> IdSet.union (av_exp e1) (IdSet.remove x (av_exp e2))
  | ERec (binds, e) ->
      IdSet.diff (IdSetExt.unions (map av_exp (e :: map thd3 binds)))
        (IdSetExt.from_list (map fst3 binds))
  | ESeq (_, e1, e2) -> IdSet.union (av_exp e1) (av_exp e2)
  | ELabel (_, _, _, e) -> av_exp e
  | EBreak (_, _, e) -> av_exp e
  | ETryCatch (_, e1, x, e2) -> 
      IdSet.union (av_exp e1) (IdSet.remove x (av_exp e2))
  | ETryFinally (_, e1, e2) -> IdSet.union (av_exp e1) (av_exp e2)
  | EThrow (_, e) -> av_exp e
  | ETypecast (_, _, e) -> av_exp e

let concat = List.concat

let rec nested_funcs (exp : exp) : exp list = match exp with
    EConst _ -> []
  | EArray (_, es) -> concat (map nested_funcs es)
  | EObject (_, ps) -> concat (map (fun (_, _, e) -> nested_funcs e) ps)
  | EThis _ -> []
  | EId _ -> []
  | EBracket (_, e1, e2) ->nested_funcs e1 @ nested_funcs e2
  | ENew (_, c, args) -> concat (map nested_funcs (c :: args))
  | EPrefixOp (_, _, e) -> nested_funcs e
  | EInfixOp (_, _, e1, e2) -> nested_funcs e1 @ nested_funcs e2
  | EIf (_, e1, e2, e3) -> concat (map nested_funcs [e1; e2; e3])
  | EApp (_, f, args) -> concat (map nested_funcs (f :: args))
  | EFunc _ -> [exp]
  | ELet (_, x, e1, e2) -> nested_funcs e1 @ nested_funcs e2
  | ERec (binds, e) -> concat  (map nested_funcs (e :: map thd3 binds))
  | ESeq (_, e1, e2) -> nested_funcs e1 @ nested_funcs e2
  | ELabel (_, _, _, e) -> nested_funcs e
  | EBreak (_, _, e) -> nested_funcs e
  | ETryCatch (_, e1, x, e2) -> nested_funcs e1 @ nested_funcs e2
  | ETryFinally (_, e1, e2) -> nested_funcs e1 @ nested_funcs e2
  | EThrow (_, e) -> nested_funcs e
  | ETypecast (_, _, e) -> nested_funcs e


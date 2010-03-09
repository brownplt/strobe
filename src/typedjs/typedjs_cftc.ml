open Prelude
open Typedjs_syntax
open Typedjs_cf
open Typedjs_lattice

module H = Hashtbl

let rec a_exp (exp : exp) : exp = match exp with
  | EConst _ -> exp
  | EArray (p, es) -> EArray (p, map a_exp es)
  | EObject (p, props) -> EObject (p, map a_prop props)
  | EThis p -> EThis p
  | EDeref (_, EId (p, x)) -> begin try
      let node = H.find bound_id_map (p, x) in
      let env = H.find envs node in
      let heap = H.find heaps node in
        match lookup x env with
          | ARef loc -> ETypecast (p, deref loc heap, exp)
          | _ -> exp
    with Not_found -> exp 
    end
  | EId (p, x) -> begin try 
      let node = H.find bound_id_map (p,x) in
      let env = H.find envs node in
        ETypecast (p, to_set (H.find heaps node) (lookup x env), exp)
    with Not_found -> exp 
    end
  | EBracket (p, e1, e2) -> EBracket (p, a_exp e1, a_exp e2)
  | ENew (p, e1, es) -> ENew (p, a_exp e1, map a_exp es)
  | EPrefixOp (p, op, e) -> EPrefixOp (p, op, a_exp e)
  | EInfixOp (p, op, e1, e2) -> EInfixOp (p, op, a_exp e1, a_exp e2)
  | EIf (p, e1, e2, e3) -> EIf (p, a_exp e1, a_exp e2, a_exp e3)
  | EApp (p, e1, es) -> EApp (p, a_exp e1, map a_exp es)
  | EFunc (p, ids, t, e) -> EFunc (p, ids, t, a_exp e)
  | ELet (p, x, e1, e2) -> ELet (p, x, a_exp e1, a_exp e2)
  | ERec (binds, e) -> ERec (map a_bind binds, a_exp e)
  | ESeq (p, e1, e2) -> ESeq (p, a_exp e1, a_exp e2)
  | ELabel (p, i, t, e) -> ELabel (p, i, t, a_exp e)
  | EBreak (p, i, e) -> EBreak (p, i, a_exp e)
  | ETryCatch (p, e1, i, e2) -> ETryCatch (p, a_exp e1, i, a_exp e2)
  | ETryFinally (p, e1, e2) -> ETryFinally (p, a_exp e1, a_exp e2)
  | EThrow (p, e) -> EThrow (p, a_exp e)
  | ETypecast (p, t, e) -> ETypecast (p, t, a_exp e)
  | ERef (p, e) -> ERef (p, a_exp e)
  | EDeref (p, e) -> EDeref (p, a_exp e)
  | ESetRef (p, e1, e2) -> ESetRef (p, a_exp e1, a_exp e2)

and a_prop (s, b, e) = (s, b, a_exp e)

and a_bind (i, t, e) = (i, t, a_exp e)

let rec a_def (def : def) : def = match def with
    DEnd -> DEnd
  | DExp (e, d) -> DExp (a_exp e, a_def d)
  | DLet (p, i, e, d) -> DLet (p, i, a_exp e, a_def d)
  | DRec (binds, d) -> DRec (map a_bind binds, a_def d)
  | DConstructor (c, d) -> 
      let c' = {c with constr_exp = a_exp c.constr_exp;
                  constr_prototype = a_exp c.constr_prototype;
                  constr_inits = map (second2 a_exp) c.constr_inits} 
      in DConstructor (c', a_def d)

let insert_typecasts  = a_def
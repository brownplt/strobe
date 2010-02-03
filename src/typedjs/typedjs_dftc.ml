open Prelude
open Typedjs_stxutil
open Typedjs_syntax
open Typedjs_dfLattice
open Typedjs_pretty
open Typedjs_df

let rec rt_of_typ (t : typ) : RTSet.t = match t with
    TArrow _ -> RTSet.singleton RTFunction
  | TUnion (t1, t2) -> RTSet.union (rt_of_typ t1) (rt_of_typ t2)
  | TApp (s, []) -> begin match s with
        "String" ->  RTSet.singleton RTString
      | "RegExp" -> RTSet.singleton RTObject
      | "Number"  -> RTSet.singleton RTNumber
      | "Int" -> RTSet.singleton RTNumber
      | "Boolean" -> RTSet.singleton RTBoolean
      | "Undefined" -> RTSet.singleton RTUndefined
      | _ -> failwith (sprintf "unknown type: TApp (\"%s\", [])" s)
    end
  | TApp _ -> failwith (sprintf "unknown type: %s" (pretty_string pretty_typ t))
  | TTop -> any_runtime_typ
  | TBot -> RTSet.empty

let runtime (t : typ) : abs_value = AVType (rt_of_typ t)

let static (rt : runtime_typs) (t : typ) : typ = failwith "NYI"

let annotate (env : typ IdMap.t) (available_ids : IdSet.t) 
    (exp : pos exp) : pos exp =
  let anfexp = Typedjs_anf.from_typedjs exp in
  let df_env = 
    IdMap.fold (fun x t env -> bind_env x (runtime t) env) 
      env empty_env in
  let _, cast_env = Typedjs_df.local_type_analysis df_env anfexp in
  let rec cast (ids : IdSet.t) (exp : pos exp) : pos exp = match exp with
      EString _ -> exp
    | ERegexp _ -> exp
    | ENum _ -> exp
    | EInt _ -> exp
    | EBool _ -> exp
    | ENull _ -> exp
    | EArray (p, es) -> EArray (p, map (cast ids) es)
    | EObject (p, props) -> EObject (p, map (second2 (cast ids)) props)
    | EThis _ -> exp
    | EId (q, x) -> if IdSet.mem x ids then 
        let v = BoundIdMap.find (x, q) cast_env in
          ETypecast (q, abs_value_to_runtime_typs v, exp)
      else
        exp
    | EBracket (p, e1, e2) -> EBracket (p, cast ids e1, cast ids e2)
    | ENew (p, e, es) -> ENew (p, cast ids e, map (cast ids) es)
    | EPrefixOp (p, op, e) -> EPrefixOp (p, op, cast ids e)
    | EInfixOp (p, op, e1, e2) -> EInfixOp (p, op, cast ids e1, cast ids e2)
    | EIf (p, e1, e2, e3) -> EIf (p, cast ids e1, cast ids e2, cast ids e3)
    | EAssign (p, LVar (q, x), e) -> EAssign (p, LVar (q, x), cast ids e)
    | EApp (p, e, es) -> EApp (p, cast ids e, map (cast ids) es)
    | EFunc _ -> exp (* intraprocedural *)
    | EUndefined _ -> exp
    | ELet (p, x, e1, e2) -> ELet (p, x, cast ids e1, cast (IdSet.add x ids) e2)
    | ERec (binds, body) -> ERec (binds, cast ids body)
    | ESeq (p, e1, e2) -> ESeq (p, cast ids e1, cast ids e2)
    | ELabel (p, l, t, e) -> ELabel (p, l, t, cast ids e)
    | EBreak (p, x, e) -> EBreak (p, x, cast ids e)
    | ETryCatch (p, e1, x, e2) -> 
        ETryCatch (p, cast ids e1, x, cast (IdSet.add x ids) e2)
    | ETryFinally (p, e1, e2) -> ETryFinally (p, cast ids e1, cast ids e2)
    | EThrow (p, e) -> EThrow (p, cast ids e)
    | ETypecast (p, t, e) -> ETypecast (p, t, cast ids e) in
    cast available_ids exp
    

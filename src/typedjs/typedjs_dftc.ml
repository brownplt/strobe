open Prelude
open Typedjs_stxutil
open Typedjs_syntax
open Typedjs_dfLattice
open Typedjs_pretty
open Typedjs_df
open Typedjs_types

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
  | TObject _ -> RTSet.singleton RTObject
  | TRef t -> rt_of_typ t
  | TTop -> any_runtime_typ
  | TBot -> RTSet.empty

let runtime (t : typ) : abs_value = AVType (rt_of_typ t)

let rec static (rt : runtime_typs) (typ : typ) : typ = match typ with
    TTop -> TTop
  | TBot -> TBot (* might change if we allow arbitrary casts *)
  | TArrow _ -> if RTSet.mem RTFunction rt then typ else TBot
  | TApp ("String", []) -> if RTSet.mem RTString rt then typ else TBot
  | TApp ("RegExp", []) -> if RTSet.mem RTObject rt then typ else TBot
  | TApp ("Number", []) -> if RTSet.mem RTNumber rt then typ else TBot
  | TApp ("Int", []) -> if RTSet.mem RTNumber rt then typ else TBot
  | TApp ("Boolean", []) -> if RTSet.mem RTBoolean rt then typ else TBot
  | TApp ("Undefined", []) -> if RTSet.mem RTUndefined rt then typ else TBot
  | TApp _ -> failwith (sprintf "unknown type in static: %s"
                          (pretty_string pretty_typ typ))
  | TObject _ -> if RTSet.mem RTObject rt then typ else TBot
  | TRef t -> TRef t
  | TUnion (s, t) -> typ_union (static rt s) (static rt t)
 
let annotate (env : Env.env) (exp : pos exp) : pos exp =
  let anfexp = Typedjs_anf.from_typedjs exp in
  let assignable_ids = Env.assignable_ids env in
  let df_env = 
    IdMap.fold (fun x t env -> 
                  if IdSet.mem x assignable_ids then 
                    bind_env x (runtime t) env
                  else
                    env)
      (Env.id_env env) empty_env in
  let cast_env = Typedjs_df.local_type_analysis df_env anfexp in
  let rec cast (ids : IdSet.t) (exp : pos exp) : pos exp = match exp with
      EString _ -> exp
    | ERegexp _ -> exp
    | ENum _ -> exp
    | EInt _ -> exp
    | EBool _ -> exp
    | ENull _ -> exp
    | EArray (p, es) -> EArray (p, map (cast ids) es)
    | EObject (p, props) -> EObject (p, map (third3 (cast ids)) props)
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
    | ERec (binds, body) -> 
        let removed_ids = IdSetExt.unions 
          (map (fun (_, _, e) -> av_exp e) binds) in
        let ids' = IdSet.diff ids removed_ids in
          ERec (binds, cast ids' body)
    | ESeq (p, e1, e2) -> ESeq (p, cast ids e1, cast ids e2)
    | ELabel (p, l, t, e) -> ELabel (p, l, t, cast ids e)
    | EBreak (p, x, e) -> EBreak (p, x, cast ids e)
    | ETryCatch (p, e1, x, e2) -> 
        ETryCatch (p, cast ids e1, x, cast (IdSet.add x ids) e2)
    | ETryFinally (p, e1, e2) -> ETryFinally (p, cast ids e1, cast ids e2)
    | EThrow (p, e) -> EThrow (p, cast ids e)
    | ETypecast (p, t, e) -> ETypecast (p, t, cast ids e) in
    cast (Env.assignable_ids env) exp
    

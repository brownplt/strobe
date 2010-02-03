open Prelude
open Typedjs_anf
open Typedjs_dfLattice
open Typedjs_syntax
open Typedjs_pretty

type bound_id = id * pos

module BoundIdOrdered = struct
  type t = bound_id
  let compare = Pervasives.compare
end

module BoundIdMap = Map.Make (BoundIdOrdered)

(******************************************************************************)

module J = JavaScript_syntax

let any_val = AVType any_runtime_typ

let single_value rt = AVType (RTSet.singleton rt)

(** The result maps nodes to their abstract values. This component of the
    result must be threaded through the analysis. The result also maps
    enclosing labels to abstract values. *)
type result = abs_value NodeMap.t * abs_value IdMap.t

let env_at_node : env NodeMap.t ref = ref NodeMap.empty

let bound_id_map : abs_value BoundIdMap.t ref = ref BoundIdMap.empty

let string_of_rt rt = 
  RTSetExt.pretty Format.str_formatter pretty_runtime_typ rt;
  Format.flush_str_formatter ()


let value (env : env) (value : pos value) = match value with
    VId (p, x) -> 
      let t = lookup_env x env in
        bound_id_map := BoundIdMap.add (x, p) t !bound_id_map;
        t
  | VString str -> AVString str
  | VNum _ -> single_value RTNumber
  | VInt _ -> single_value RTNumber
  | VRegexp _ -> single_value RTObject
  | VBool _ -> single_value RTBoolean
  | VNull -> single_value RTObject
  | VArray _ -> single_value RTObject
  | VObject _ -> single_value RTObject
  | VThis _ -> single_value RTObject
  | VFunc _ -> single_value RTFunction
  | VUndefined -> single_value RTUndefined

let mk_type_is x s = match s with
    "string" -> AVTypeIs (x, RTSet.singleton RTString)
  | "number" -> AVTypeIs (x, RTSet.singleton RTNumber)
  | "boolean" -> AVTypeIs (x, RTSet.singleton RTBoolean)
  | "function" -> AVTypeIs (x, RTSet.singleton RTFunction)
  | "object" -> AVTypeIs (x, RTSet.singleton RTObject)
  | "undefined" -> AVTypeIs (x, RTSet.singleton RTUndefined)
  | _ -> single_value RTBoolean

let bindexp (env : env) (exp : pos bind) : abs_value * env = match exp with
    BValue v -> (value env v, env)
  | BApp _ -> (any_val, env)
  | BBracket _ -> (any_val, env)
  | BNew _ -> (any_val, env)
  | BPrefixOp (J.PrefixTypeof, v) -> 
      let _ = value env v in
        begin match v with
            VId (_, x) -> AVTypeof x, env
          | _ -> any_val, env
        end
  | BInfixOp (op, v1, v2) -> 
      begin match op, value env v1, value env v2 with
          J.OpStrictEq, AVTypeof x, AVString s -> (mk_type_is x s , env)
        | J.OpStrictEq, AVString s,  AVTypeof x  -> (mk_type_is x s, env)
        | J.OpStrictEq, _, _ -> single_value RTBoolean, env
        | _ -> any_val, env
      end
  | BAssign (x, v) -> 
      let av = value env v in
        (av, bind_env x av env)
  | BSetProp (_, _, v) ->
      (value env v, env) (* obj.x = v returns v *)
  | BIf _ -> failwith "typedjs_df.ml : BIf should be handled in anfexp"


let rec anfexp (env : env) (exp : pos anfexp) : result =  
  env_at_node := NodeMap.add (node_of_anfexp exp) env !env_at_node;
  match exp with
    ALet (node, x, BIf (v1, e2, e3), body) ->
      let env2, env3 = match value env v1 with
          AVTypeIs (x, rt) -> 
            let x_rt = abs_value_to_runtime_typs (lookup_env x env) in
              (bind_env x (AVType rt) env, 
               bind_env x (AVType (RTSet.diff x_rt rt)) env)
        | _ -> env, env in
        
      let vals2, label_env2 = anfexp env2 e2 in
      let vals3, label_env3 = anfexp env3 e3 in
      let vals = NodeMapExt.join union_abs_value vals2 vals3 in
      let label_env = IdMapExt.join union_abs_value label_env2 label_env3 in
      let x_val = union_abs_value
        (NodeMap.find (node_of_anfexp e2) vals)
        (NodeMap.find (node_of_anfexp e3) vals) in
      let vals', label_env' = anfexp (bind_env x x_val env) body in
        (NodeMap.add node (NodeMap.find (node_of_anfexp body) vals') vals',
         IdMapExt.join union_abs_value label_env label_env')
  | ALet (node, x, b, e) ->
      let (absval, env') = bindexp env b in
      let (vals, label_env) = anfexp (bind_env x absval env') e in
        (NodeMap.add node (NodeMap.find (node_of_anfexp e) vals) vals,
         label_env)
  | ARec (node, binds, body) ->
      (* TODO: assumes that everything in a rec block is a function! *)
      let add_func env x = bind_env x (single_value RTFunction) env in
      let env' = fold_left add_func env (map fst2 binds) in
      let (vals, label_env) = anfexp env' body in
        (NodeMap.add node (NodeMap.find (node_of_anfexp body) vals) vals,
         label_env)
         
  | ALabel (n, x, e) ->
      let vals, label_env = anfexp env e in
      let absval = 
        if IdMap.mem x label_env then 
          union_abs_value
            (IdMap.find x label_env) 
            (NodeMap.find (node_of_anfexp e) vals)
        else 
          NodeMap.find (node_of_anfexp e) vals in
      NodeMap.add n absval vals, IdMap.remove x label_env
  | ABreak (n, x, v) -> 
      (NodeMap.add n (AVType RTSet.empty) NodeMap.empty, 
       IdMap.add x (value env v) IdMap.empty)
  | AValue (n, v) ->
      (NodeMap.add n (value env v) NodeMap.empty, IdMap.empty)

let local_type_analysis env exp =
  env_at_node := NodeMap.empty;
  bound_id_map := BoundIdMap.empty;
  let vals, _ = anfexp env exp in
    !env_at_node, !bound_id_map

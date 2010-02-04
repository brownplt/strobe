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

let string_of_rt rt = 
  RTSetExt.pretty Format.str_formatter pretty_runtime_typ rt;
  Format.flush_str_formatter ()

let bound_id_map : abs_value BoundIdMap.t ref = ref BoundIdMap.empty

let value (env : env) (value : pos value) = match value with
    VId (p, x) -> 
      if env_binds x env then
        let t = lookup_env x env in
          bound_id_map := BoundIdMap.add (x, p) t !bound_id_map;
          t
      else
        any_val
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
            VId (_, x) when env_binds x env -> AVTypeof x, env
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

(** The result maps nodes to their abstract values. This component of the
    result must be threaded through the analysis. The result also maps
    enclosing labels to abstract values. *)
type result = abs_value IdMap.t



let local_type_analysis env exp =
  bound_id_map := BoundIdMap.empty;
  let node_values : abs_value NodeMap.t ref = ref NodeMap.empty in
  let node_value (e : 'a anfexp) : abs_value = 
    NodeMap.find (node_of_anfexp e) !node_values in
  let set_node_value (n : node) (v : abs_value) : unit =
    node_values := NodeMap.add n v !node_values in

  let rec anfexp (env : env) (exp : pos anfexp) : result = match exp with
      ALet (node, x, BIf (v1, e2, e3), body) ->
        let env2, env3 = match value env v1 with
            AVTypeIs (x, rt) -> 
              let x_rt = abs_value_to_runtime_typs (lookup_env x env) in
              let x_false = RTSet.diff x_rt rt in 
                (bind_env x (AVType rt) env, 
                 bind_env x (AVType x_false) env)
          | _ -> env, env in
          
        let label_env2 = anfexp env2 e2 in
        let label_env3 = anfexp env3 e3 in
        let u v1 v2 = if v1 = v2 then v1 else begin
          union_abs_value v1 v2
        end in
        let label_env = IdMapExt.join u label_env2 label_env3 in
        let x_val = union_abs_value (node_value e2) (node_value e3) in
        let label_env' = anfexp (bind_env x x_val label_env) body in
          set_node_value node (node_value body);
          IdMapExt.join union_abs_value label_env label_env'
    | ALet (node, x, b, e) ->
        let (absval, env') = bindexp env b in
        let label_env = anfexp (bind_env x absval env') e in
          set_node_value node (node_value e);
          label_env
    | ARec (node, binds, body) ->
        (* TODO: assumes that everything in a rec block is a function! *)
        let add_func env x = bind_env x (single_value RTFunction) env in
        let env' = fold_left add_func env (map fst2 binds) in
        let label_env = anfexp env' body in
          set_node_value node (node_value body);
          label_env
    | ALabel (n, x, e) ->
        let label_env = anfexp env e in
        let absval = 
          if IdMap.mem x label_env then 
            union_abs_value
              (IdMap.find x label_env)
              (node_value e)
          else 
            (node_value e) in
          IdMap.remove x label_env
    | ABreak (node, x, v) -> 
        set_node_value node (AVType RTSet.empty);
        IdMap.add x (value env v) env
    | AValue (node, v) ->
        set_node_value node (value env v);
        env

  in let  _ = anfexp env exp in
    !bound_id_map








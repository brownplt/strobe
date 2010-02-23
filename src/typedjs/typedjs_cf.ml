open Prelude
open Typedjs_cps
open Typedjs_dfLattice

module H = Hashtbl
module J = JavaScript_syntax

let single t = AVType (RTSet.singleton t)

let any_val = AVType any_runtime_typ


(* The abstract environment at each node lets us lookup the abstract value
   of bound identifiers and perform abstract operations. *)
let envs : (node, env) H.t = H.create 200

(* We map names of lexically-known functions to their formal arguments and
   function bodies. The analysis is interprocedural when applying known
   functions. *)
type known = (id list * cpsexp) IdMap.t

let set_env (n : node)


let mk_type_is x s = match s with
    "string" -> AVTypeIs (x, RTSet.singleton RTString)
  | "number" -> AVTypeIs (x, RTSet.singleton RTNumber)
  | "boolean" -> AVTypeIs (x, RTSet.singleton RTBoolean)
  | "function" -> AVTypeIs (x, RTSet.singleton RTFunction)
  | "object" -> AVTypeIs (x, RTSet.singleton RTObject)
  | "undefined" -> AVTypeIs (x, RTSet.singleton RTUndefined)
  | _ -> single_value RTBoolean

let abs_of_cpsval (env : env) (cpsval : cpsval) : abs_value = 
  match cpsval with
      Const (_, c) -> begin match c with
          CString s -> AVString s
        | CRegexp _ -> single RTObject
        | CNum _ -> single RTNumber
        | CInt _ -> single RTNumber
        | CBool _ -> single RTBoolean
        | CNull -> single RTObject
        | CUndefined -> single RTUndefined
      end
    | Array _ -> single RTObject
    | Object _ -> single RTObject
    | Id x -> lookup_env x env

let rec calc (env : env) (cpsexp : cpsexp) = match cpsexp with
    Let0 (n, x, v, e) -> 
      let r = abs_of_cpsval env v in
        flow (bind_env x r env) e
  | Let1 (, x, op, v, e) -> 
      let r = match op, abs_of_cpsval env v with
          J.PrefixTypeof, Id x -> AVTypeof x
        | _ -> any_val in
        flow (bind_env x r env) e
  | Let2 (_, x, op, v1, v2, e) ->
      let r = 
        match op, abs_of_cpsval env v1, abs_of_cpsval env env v2 with
            J.OpStrictEq, AVTypeof x, AVString s -> mk_type_is x s
        | J.OpStrictEq, AVString s,  AVTypeof x  -> mk_type_is x s
        | J.OpStrictEq, _, _ -> single RTBoolean
        | _ -> any_val in
        flow (bind_env x r env) e
  | Array (_, x, elts, k) ->
      let elts' = map (abs_of_cpsval env) elts in
      let env' = bind x  [[ array of elts' ]] env in
        flow env' k
  | Assign (_, x, v, e) ->
      let r = abs_of_cpsval env v in
        flow (bind_env x r env) e (* TODO: certainly wrong *)
  | If (_, v1, e2, e3) ->
      let env2, env3 = match v1 with
          AVTypeIs (x, rt) -> 
            let x_rt = abs_value_to_runtime_typs (lookup_env x env) in
            let x_false = RTSet.diff x_rt rt in 
              bind_env x (AVType rt) env, bind_env x (AVType x_false) env
        | _ -> env, env in
        flow env2 e2;
        flow env3 e3
  | Fix (_, binds, body) ->
      let esc = esc_cpsexp exp in
      let is_escaping (x, _, _, _) = IdSet.mem x esc in
      let esc_binds, nonesc_binds = List.partition is_escaping binds in
        (* TODO: we assume that non-escaping binds are actually applied. *)

      let close (f, formals, _, body) = ... CONTINUE HERE ... 
          

  | App (_, f, args) ->
      begin match abs_of_cpsval env f, map (abs_of_cpsval env) args with
          Id f', args' when IdMap.mem f' known ->
            flow 


and flow (env : env) (cpsexp : cpsexp) = ...

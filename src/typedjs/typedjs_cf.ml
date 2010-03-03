open Prelude
open Typedjs_cps
open Typedjs_lattice

module H = Hashtbl
module J = JavaScript_syntax

(* The abstract environment at each node lets us lookup the abstract value
   of bound identifiers and perform abstract operations. *)
let envs : (node, env) H.t = H.create 200

let bound_id_map : (pos * id, node) H.t = H.create 200

let mk_type_is x s = match s with
  | "string" -> ATypeIs (x, RTSet.singleton RT.String)
  | "number" -> ATypeIs (x, RTSet.singleton RT.Number)
  | "boolean" -> ATypeIs (x, RTSet.singleton RT.Boolean)
  | "function" -> ATypeIs (x, RTSet.singleton RT.Function)
  | "object" -> ATypeIs (x, RTSet.singleton RT.Object)
  | "undefined" -> ATypeIs (x, RTSet.singleton RT.Undefined)
  | _ -> singleton RT.Boolean

let abs_of_cpsval node env (cpsval : cpsval) = match cpsval with
  | Const c -> begin match c with
      | Exprjs_syntax.CString s -> AString s
      | Exprjs_syntax.CRegexp _ -> singleton RT.Object
      | Exprjs_syntax.CNum _ -> singleton RT.Number
      | Exprjs_syntax.CInt _ -> singleton RT.Number
      | Exprjs_syntax.CBool _ -> singleton RT.Boolean
      | Exprjs_syntax.CNull -> singleton RT.Object
      | Exprjs_syntax.CUndefined -> singleton RT.Undefined
    end
  | Id (p, x) -> 
      H.replace bound_id_map (p, x) node;
      let rt = lookup x env in
        eprintf "Runtime type of %s is %s\n" x (FormatExt.to_string p_av rt);
        rt

let rec calc (env : env) (cpsexp : cpsexp) = match cpsexp with
  | Let0 (n, x, v, cont) -> 
      let r = abs_of_cpsval n env v in
        flow (bind x r env) cont
  | Let1 (n, x, op, v, e) -> 
      let v' = abs_of_cpsval n env v in
      let r = match op, v with
        | J.PrefixTypeof, Id (_, x) -> ATypeof x
        | _ -> any in
        flow (bind x r env) e
  | Let2 (n, x, op, v1, v2, e) ->
      let r = 
        match op, abs_of_cpsval n env v1, abs_of_cpsval n env v2 with
            J.OpStrictEq, ATypeof x, AString s -> mk_type_is x s
        | J.OpStrictEq, AString s,  ATypeof x  -> mk_type_is x s
        | J.OpStrictEq, _, _ -> singleton RT.Boolean
        | _ -> any in
        flow (bind x r env) e
  | If (n, v1, e2, e3) ->
      let env2, env3 = match abs_of_cpsval n env v1 with
        | ATypeIs (x, x_true) -> 
            let x_false = RTSet.diff (to_set (lookup x env)) x_true in 
              bind x (ASet x_true) env, bind x (ASet x_false) env
        | _ -> env, env in
        flow env2 e2;
        flow env3 e3
  | Fix (n, binds, cont) ->
      let esc_set = esc_cpsexp cpsexp in
      let is_escaping (f, _, _, _) = IdSet.mem f esc_set in
      let (esc_binds, nonesc_binds) = List.partition is_escaping binds in
      let env' = fold_left bind_lambda env nonesc_binds in
      let env' = fold_left bind_esc_lambda env' esc_binds in
        List.iter (mk_closure env') nonesc_binds;
        flow env' cont; 
        List.iter (sub_flow env') esc_binds
  | App (n, f, args) ->
      begin match abs_of_cpsval n env f, map (abs_of_cpsval n env) args with
        | AClosure (_, formals, body), argvs -> 
            let flow_env = List.fold_right2 bind formals argvs empty_env in
              flow flow_env body
        | _ -> eprintf "applying an escaped function\n"
      end

and bind_lambda env (f, args, typ, body_exp) =
  bind f (AClosure (node_of_cpsexp body_exp, args, body_exp)) env

and bind_esc_lambda env (f, _,_, _) =
  bind f (singleton RT.Function) env

and mk_closure (env : env) (f, args, typ, body_exp) =
  let node = node_of_cpsexp body_exp in
    try ignore (H.find envs node)
    with Not_found -> H.replace envs node env

and sub_flow (env : env) (f, args, typ, body_exp) =  match typ with
    Typedjs_syntax.TArrow (_, arg_typs, _) -> 
      eprintf "sub_flow into %s\n" f;
      let arg_avs = map runtime arg_typs in
      let flow_env = List.fold_right2 bind args arg_avs env in
        flow flow_env body_exp
  | _ -> failwith "expected TArrow in sub_flow"

and flow (env : env) (cpsexp : cpsexp) = 
  let node = node_of_cpsexp cpsexp in
    eprintf "Flowing to %d... " node;
  let old_env, reflow  = 
    try H.find envs node, false
    with Not_found -> empty_env, true in
  let new_env = union_env old_env env in
    if Pervasives.compare old_env new_env != 0 || reflow then
      begin
        eprintf "calc\n";
        H.replace envs node new_env;
        calc new_env cpsexp
      end
    else eprintf "not flowing\n"
              
let typed_cfa (env : env) (cpsexp : cpsexp) : unit =
  flow env cpsexp

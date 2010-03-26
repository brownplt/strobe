open Prelude
open Typedjs_cps
open Typedjs_lattice
open Typedjs_syntax

module H = Hashtbl
module J = JavaScript_syntax
module Heap = Map.Make (Loc)

(* The abstract environment at each node lets us lookup the abstract value
   of bound identifiers and perform abstract operations. *)
let envs : (node, env) H.t = H.create 200

let heaps : (node, heap) H.t = H.create 200

let bound_id_map : (pos * id, node) H.t = H.create 200

let reached_nodes : (node, bool) H.t = H.create 200

let lambdas : (node, node * env * cpsexp) H.t = H.create 200

let subflows : (node, (bool ref * node * env * cpsexp)) H.t = H.create 200

let global_heap = ref empty_heap

let mk_type_is x s = match s with
  | "string" -> ALocTypeIs (x, RTSet.singleton RT.String)
  | "number" -> ALocTypeIs (x, RTSet.singleton RT.Number)
  | "boolean" -> ALocTypeIs (x, RTSet.singleton RT.Boolean)
  | "function" -> ALocTypeIs (x, RTSet.singleton RT.Function)
  | "object" -> ALocTypeIs (x, RTSet.singleton RT.Object)
  | "undefined" -> ALocTypeIs (x, RTSet.singleton RT.Undefined)
  | _ -> singleton RT.Boolean

let abs_of_cpsval node env (cpsval : cpsval) = match cpsval with
  | Const c -> begin match c with
      | JavaScript_syntax.CString s -> AString s
      | JavaScript_syntax.CRegexp _ -> singleton RT.Object
      | JavaScript_syntax.CNum _ -> singleton RT.Number
      | JavaScript_syntax.CInt _ -> singleton RT.Number
      | JavaScript_syntax.CBool _ -> singleton RT.Boolean
      | JavaScript_syntax.CNull -> singleton RT.Object
      | JavaScript_syntax.CUndefined -> singleton RT.Undefined
    end
  | Id (p, x) -> 
      H.replace bound_id_map (p, x) node;
      lookup x env

let calc_op1 node env heap (op : op1)  v = match op, v with
  | Ref, v -> 
      let loc = Loc node in
        ARef loc, set_ref loc (to_set heap v) heap
  | Deref, ARef loc ->
      ADeref loc, heap
  | Op1Prefix J.PrefixTypeof, ADeref loc -> 
      ALocTypeof loc, heap
  | _ -> any, heap

let calc_op2 node env heap op v1 v2 = match op, v1, v2 with
  | Op2Infix J.OpStrictEq, ALocTypeof loc, AString str ->
      mk_type_is loc str, heap
  | SetRef, ARef l, v ->
      v, set_ref l (to_set heap v) heap
  | _ -> any, heap

let mk_closure (env : env) (f, args, typ, body_exp) =
  let node = node_of_cpsexp body_exp in
    try ignore (H.find envs node)
    with Not_found -> H.replace envs node env

let rec calc (env : env) (heap : heap) (cpsexp : cpsexp) = match cpsexp with
  | Bind (node, x, bindexp, cont) ->
      let cpsval, heap = match bindexp with
        | Let v -> abs_of_cpsval node env v, heap
        | Op1 (op, v) -> calc_op1 node env heap op (abs_of_cpsval node env v)
        | Op2 (op, v1, v2) ->
            calc_op2 node env heap op 
              (abs_of_cpsval node env v1) 
              (abs_of_cpsval node env v2)
        | Object _ -> singleton RT.Object, heap
        | Array _ -> singleton RT.Object, heap
        | UpdateField _ -> singleton RT.Object, heap in
        flow (bind x cpsval env) heap cont
  | If (node, v1, true_cont, false_cont) ->
      let heap2, heap3 = match abs_of_cpsval node env v1 with
        | ALocTypeIs (loc, true_set) ->
            (set_ref loc true_set heap,
             let false_set = RTSet.diff (deref loc heap) true_set in
               set_ref loc false_set heap)
        | _ -> heap, heap in
        flow env heap2 true_cont;
        flow env heap3 false_cont
  | Fix (n, binds, cont) ->
      let esc_set = esc_cpsexp cpsexp in
      let is_escaping (f, _, _, _) = IdSet.mem f esc_set in
      let (esc_binds, nonesc_binds) = List.partition is_escaping binds in
      let env' = fold_left bind_lambda env nonesc_binds in
      let env' = fold_left bind_esc_lambda env' esc_binds in
        List.iter (mk_closure env') nonesc_binds;
        flow env' heap cont; 
        List.iter (sub_flow (node_of_cpsexp cont)) binds
  | App (n, f, args) ->
      begin match abs_of_cpsval n env f, map (abs_of_cpsval n env) args with
        | AClosure (_, formals, body), argvs -> 
            let flow_env = List.fold_right2 bind formals argvs empty_env in
              flow flow_env heap body
        | _ -> ()
      end

and bind_lambda env (f, args, typ, body_exp) =
  bind f (AClosure (node_of_cpsexp body_exp, args, body_exp)) env

and bind_esc_lambda env (f, _,_, _) =
  bind f (singleton RT.Function) env

and sub_flow (env_node : node) (f, args, typ, body_exp) =  match typ with
  | Typedjs_syntax.TArrow (_, arg_typs, _) -> begin try
      ignore (H.find lambdas (node_of_cpsexp body_exp))
    with Not_found ->
      H.add lambdas (node_of_cpsexp body_exp)
        (env_node, 
         List.fold_right2 bind args (map runtime arg_typs) empty_env,
         body_exp)
    end
  | _ -> failwith "expected TArrow in sub_flow"

and flow (env : env) (heap : heap) (cpsexp : cpsexp) = 
  let node = node_of_cpsexp cpsexp in
  let old_env, reflow  = 
    try H.find envs node, false
    with Not_found -> empty_env, true in
  let old_heap, reflow =
    try H.find heaps node, reflow 
    with Not_found -> empty_heap, true in
  let new_heap = union_heap old_heap heap in
  let new_env = union_env new_heap old_env env in
    if (compare_env old_env new_env != 0 || 
        compare_heap old_heap new_heap != 0 ||
        reflow) then
      begin
        H.replace reached_nodes node true;
        H.replace envs node new_env;
        H.replace heaps node new_heap;
        calc new_env new_heap cpsexp
      end
    else ()
              
let typed_cfa (env : env) (cpsexp : cpsexp) : unit =
  flow env empty_heap cpsexp;
  let rec sub_flows () =
    if H.length lambdas > 0 then 
      let sub node (env_node, arg_env, exp) =
        H.remove lambdas node;
        if not (H.mem reached_nodes node) then 
          begin
            let heap = escape_heap (H.find heaps env_node) in
            let env = escape_env heap (H.find envs env_node) in
              H.remove envs node;
              (* eprintf "%d: restarting flow with this env:\n%s\nand this heap:\n%s\n" node (FormatExt.to_string p_env env) (FormatExt.to_string p_heap heap); *)
              flow (union_env heap arg_env env) heap exp
          end in
        H.iter sub lambdas;

        sub_flows () (* subflows may add lambdas *)
    else
      () in
    sub_flows ()
        
        

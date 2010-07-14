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

let op_env = ref IdMap.empty

let set_op_env env = 
  op_env := Typedjs_env.operator_env_of_tc_env env

let tag_of_string s = match s with
  | "string" -> RT.Str
  | "number" -> RT.Num
  | "boolean" -> RT.Bool
  | "function" ->  RT.Function
  | "object" -> RT.Object
  | "undefined" ->  RT.Undefined
  | _ -> raise (Invalid_argument "tag_of_string")

let mk_type_is x s = 
  try 
    ALocTypeIs (x, RTSet.singleton (tag_of_string s))
  with Invalid_argument "tag_of_string" ->
    singleton RT.Bool

let mk_type_is_not x s =
  try 
    ALocTypeIs (x, RTSet.remove (tag_of_string s) rtany)
  with Invalid_argument "tag_of_string" ->
    singleton RT.Bool  

let abs_of_cpsval node env (cpsval : cpsval) = match cpsval with
  | Const c -> begin match c with
      | JavaScript_syntax.CString s -> AStr s
      | JavaScript_syntax.CRegexp _ -> singleton RT.Object
      | JavaScript_syntax.CNum _ -> singleton RT.Num
      | JavaScript_syntax.CInt _ -> singleton RT.Num
      | JavaScript_syntax.CBool b -> ABool b
      | JavaScript_syntax.CNull -> singleton RT.Object
      | JavaScript_syntax.CUndefined -> singleton RT.Undefined
    end
  | Id (p, x) -> 
      H.replace bound_id_map (p, x) node;
      lookup x env

let calc_op1 node env heap (op : op1) v = match op, v with
  | Ref, v -> 
      let loc = Loc node in
        ARef loc, set_ref loc (to_set v) heap
  | Deref, ARef loc -> (ADeref (loc, deref loc heap), heap)
  | Op1Prefix "prefix:typeof", ADeref (loc, _) -> ALocTypeof loc, heap
  | _ -> any, heap


let calc_op2 node env heap op v1 v2 = match op, v1, v2 with
  | Op2Infix "==", ALocTypeof loc, AStr str
  | Op2Infix "===", ALocTypeof loc, AStr str
  | Op2Infix "==", AStr str, ALocTypeof loc
  | Op2Infix "===",  AStr str, ALocTypeof loc ->
      (mk_type_is loc str, heap)
  | Op2Infix "!=", ALocTypeof loc, AStr str
  | Op2Infix "!==", ALocTypeof loc, AStr str
  | Op2Infix "!=", AStr str, ALocTypeof loc
  | Op2Infix "!==",  AStr str, ALocTypeof loc ->
      mk_type_is_not loc str, heap
  | SetRef, ARef l, v ->
      v, set_ref l (to_set v) heap
  | Op2Infix "+", _, _ -> 
      ASet (RTSetExt.from_list [ RT.Num; RT.Str ]), heap
  | Op2Infix op, v1, v2 -> begin try
      let fn = IdMap.find op !op_env in
        (fn [ v1; v2 ], heap)
    with Not_found ->
      failwith (sprintf "operator %s is unbound in calc_op2" op)
    end
  | _ -> any, heap

let mk_closure (env : env) (_, f, args, typ, body_exp) =
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
        flow node (bind x cpsval env) heap cont
  | If (node, v1, true_cont, false_cont) ->
      let absv1 = abs_of_cpsval node env v1 in
      let heap2, heap3, split, true_set, false_set = match absv1 with
        | ALocTypeIs (loc, true_set) ->
            let false_set = RTSet.diff (deref loc heap) true_set in
              (set_ref loc true_set heap,
               set_ref loc false_set heap,
               true, true_set, false_set)
        | _ -> 
            (heap, heap, false, RTSet.empty, RTSet.empty) in
        (match absv1 with
             ABool false -> ()
           | _ -> flow node env heap2 true_cont);
        (match absv1 with
           | ABool true -> ()
           | _ -> flow node env heap3 false_cont)
  | Fix (n, binds, cont) ->
      let esc_set = esc_cpsexp cpsexp in
      let is_escaping (boundary, f, _, _, _) = 
        boundary || IdSet.mem f esc_set in
      let (esc_binds, nonesc_binds) = List.partition is_escaping binds in
      let env' = fold_left bind_lambda env nonesc_binds in
      let env' = fold_left bind_esc_lambda env' esc_binds in
        List.iter (mk_closure env') nonesc_binds;
        flow n env' heap cont; 
        List.iter (sub_flow (node_of_cpsexp cont)) binds
  | App (n, f, args) ->
      begin match abs_of_cpsval n env f, map (abs_of_cpsval n env) args with
        | AClosure (_, formals, body), argvs -> 
              let flow_env = List.fold_right2 bind formals argvs empty_env in
                flow n flow_env heap body
        | _ -> ()
      end

and bind_lambda env (_, f, args, typ, body_exp) =
  bind f (AClosure (node_of_cpsexp body_exp, args, body_exp)) env

and bind_esc_lambda env (_, f, _,_, _) =
  bind f (singleton RT.Function) env

and sub_flow (env_node : node) (_, f, args, typ, body_exp) = 
  match Typedjs_syntax.Typ.match_func_typ typ with
    |  Some (arg_typs, _) -> begin try
         ignore (H.find lambdas (node_of_cpsexp body_exp))
       with Not_found ->
         H.add lambdas (node_of_cpsexp body_exp)
           (env_node, 
            List.fold_right2 bind args (map runtime arg_typs) empty_env,
            body_exp)
       end
    | _ -> failwith "expected TArrow in sub_flow"

and flow prev_node (env : env) (heap : heap) (cpsexp : cpsexp) = 
  let node = node_of_cpsexp cpsexp in
  let old_env, reflow  = 
    try H.find envs node, false
    with Not_found -> empty_env, true in
  let old_heap, reflow =
    try H.find heaps node, reflow 
    with Not_found -> empty_heap, true in
  let new_heap = union_heap old_heap heap in
  let new_env = union_env old_env env in
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
  flow (-1) env empty_heap cpsexp;
  let rec sub_flows () =
    if H.length lambdas > 0 then 
      let sub node (env_node, arg_env, exp) =
        H.remove lambdas node;
        if not (H.mem reached_nodes node) then 
          begin
            let heap = escape_heap (H.find heaps env_node) in
            let env = escape_env heap (H.find envs env_node) in
              H.remove envs node;
              flow (-1) (union_env arg_env env) heap exp
          end in
        H.iter sub lambdas;

        sub_flows () (* subflows may add lambdas *)
    else
      () in
    sub_flows ()
        
        

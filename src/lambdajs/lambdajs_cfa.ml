open FormatExt
open Prelude
open Lambdajs_cps
open Lambdajs_lattice
open AV
module H = Hashtbl

let reachable = H.create 100

let call_graph = H.create 500

let envs : (int, env) H.t = H.create 500

type context = int

let heaps = H.create 500

let heap_upd (heap : heap) (loc : loc) (v : AVSet.t) : heap = 
  try 
    let current_v = Heap.find loc heap in
      Heap.add loc (AVSet.union current_v v) heap
  with 
      Not_found -> Heap.add loc v heap


(* raises Not_found *)
let get_env (n : int) = Hashtbl.find envs n

let set_env (n : int) (env : env) = Hashtbl.replace envs n env

let call_from_to (m : int) (n : int) : unit = 
  if H.mem call_graph m then
    begin
      let set = H.find call_graph m in
        if not (IntSet.mem n set) then
          H.replace call_graph m (IntSet.add n set)
    end
  else
    H.add call_graph m (IntSet.singleton n)

open Lambdajs_syntax

let calc_op1 h (n : int) (op1 : op1) (avs : AVSet.t) = match op1 with
  | Ref -> 
      let loc = Loc n in
      AVSet.singleton (ARef loc), Heap.add loc avs h
  | Deref ->
      let fn v r = match v with
        | ARef loc -> AVSet.union (Heap.find loc h) r
        | _ -> r in
        AVSet.fold fn avs AVSet.empty, h
  | Op1Prefix jsOp -> 
      (match jsOp with
         | JavaScript_syntax.PrefixLNot -> AVSet.singleton ANumber
         | JavaScript_syntax.PrefixBNot -> AVSet.singleton ABool
         | JavaScript_syntax.PrefixPlus -> AVSet.singleton ANumber
         | JavaScript_syntax.PrefixMinus -> AVSet.singleton ANumber
         | JavaScript_syntax.PrefixTypeof  -> AVSet.singleton AString
         | JavaScript_syntax.PrefixVoid ->
             AVSet.singleton (AConst Exprjs_syntax.CUndefined)),
             h

let rec get_fields h obj_fields field_names : AVSet.t = match field_names with
  | [] -> AVSet.empty
  | field :: rest ->
      if not (IdMap.mem field obj_fields) then
        AVSet.add (AConst Exprjs_syntax.CUndefined) 
          (get_fields h obj_fields rest)
      else 
        let field_loc = IdMap.find field obj_fields in
        let field_val = Heap.find field_loc h in
          AVSet.union field_val  (get_fields h obj_fields rest)

let calc_op2 h op2 (vs1 : AVSet.t) (vs2 : AVSet.t) = match op2 with
  | GetField ->
      if AVSet.mem AString vs2 then
        failwith "get all fields"
      else 
        let fields_to_get = 
          AVSet.fold (fun v r -> match v with 
                        | AConst (Exprjs_syntax.CString s) -> IdSet.add s r
                        | _ ->  r) 
            vs2 IdSet.empty in
        let fields_to_get = IdSetExt.to_list fields_to_get in
          AVSet.fold (fun v r -> match v with
                        | AObj props ->
                            AVSet.union (get_fields h props fields_to_get) r
                        | _ ->  r)
            vs1 AVSet.empty, h
  | SetRef -> 
      (* return previous value, vs2 *)
      let fn v h = match v with
        | ARef loc -> Heap.add loc vs2 h
        | _ -> h in
      vs2, AVSet.fold fn vs1 h
  | Op2Infix infixOp -> 
      (match infixOp with
      | JavaScript_syntax.OpLT -> AVSet.singleton ANumber
      | JavaScript_syntax.OpLEq  -> AVSet.singleton ANumber
      | JavaScript_syntax.OpGT  -> AVSet.singleton ANumber
      | JavaScript_syntax.OpGEq   -> AVSet.singleton ANumber
      | JavaScript_syntax.OpIn -> AVSet.singleton ABool
      | JavaScript_syntax.OpInstanceof -> AVSet.singleton ABool
      | JavaScript_syntax.OpEq -> AVSet.singleton ABool
      | JavaScript_syntax.OpNEq -> AVSet.singleton ABool
      | JavaScript_syntax.OpStrictEq -> AVSet.singleton ABool
      | JavaScript_syntax.OpStrictNEq -> AVSet.singleton ABool
      | JavaScript_syntax.OpLAnd -> AVSet.singleton ABool
      | JavaScript_syntax.OpLOr -> AVSet.singleton ABool
      | JavaScript_syntax.OpMul -> AVSet.singleton ANumber
      | JavaScript_syntax.OpDiv -> AVSet.singleton ANumber
      | JavaScript_syntax.OpMod -> AVSet.singleton ANumber
      | JavaScript_syntax.OpSub -> AVSet.singleton ANumber
      | JavaScript_syntax.OpLShift -> AVSet.singleton ANumber
      | JavaScript_syntax.OpSpRShift -> AVSet.singleton ANumber
      | JavaScript_syntax.OpZfRShift -> AVSet.singleton ANumber
      | JavaScript_syntax.OpBAnd -> AVSet.singleton ANumber
      | JavaScript_syntax.OpBXor -> AVSet.singleton ANumber
      | JavaScript_syntax.OpBOr -> AVSet.singleton ANumber
      | JavaScript_syntax.OpAdd -> AVSet.singleton ANumber), h

let obj_values h obj = match obj with
  | AObj locs -> 
      IdMap.fold
        (fun fname floc dict -> IdMap.add fname (Heap.find floc h) dict)
        locs IdMap.empty
  | _ -> IdMap.empty

let calc_op3 h n obj field_name field_value =
  if AVSet.mem AString field_name then
    failwith "set all fields"
  else 
    (* Map from field name to value-sets. This maps the fields of all objects
       to all their values. *)
    let dict = AVSet.fold 
      (fun obj dict -> IdMapExt.join AVSet.union (obj_values h obj) dict)
      obj IdMap.empty in
    let dict = AVSet.fold
      (fun fname dict -> match fname with
         | AConst (Exprjs_syntax.CString s) ->
             IdMap.add s field_value dict
         | _ -> dict )
      field_name dict in
    let alloc_field fname fval (absfields, h) = 
      let loc = LocField (n, fname) in
        (IdMap.add fname loc absfields, Heap.add loc fval h) in
    let absfields, h = IdMap.fold alloc_field dict (IdMap.empty, h) in
      (AVSet.singleton (AObj absfields), h)

let rec absval (env : env) (cpsval : cpsval) : AVSet.t = match cpsval with
  | Const c -> AVSet.singleton (AV.AConst c)
  | Id "#end" -> AVSet.empty
  | Id x -> 
      try IdMap.find x env
      with Not_found -> failwith ("unbound ads dentifier " ^ x)


let rec calc (env : env) (heap : heap) cpsexp : unit = match cpsexp with
  | Fix (node, binds, body) ->
      let env' = fold_left (bind_lambda (fst2 node)) env binds in
        List.iter (mk_closure  env') binds;
        flow env' heap body
  | App ((app_n, _), f, args) ->
      let argvs = map (absval env) args in
      let do_app fv = match fv with 
        | AClosure (n, formals, body) -> (* TODO: arity *)
            call_from_to app_n (cpsexp_idx body);
            let body_env =
              List.fold_right2 IdMap.add formals argvs IdMap.empty in
             flow (union_env body_env (get_env n)) heap body
        | _ -> ()
      in AVSet.iter do_app (absval env f)

  | If (_, v1, e2, e3) -> 
      let av1 = absval env v1 in
        if AVSet.mem (AConst (Exprjs_syntax.CBool true)) av1 then
          flow env heap e2;
        if AVSet.mem (AConst (Exprjs_syntax.CBool false)) av1 then
          flow env heap e3
  | Bind ((n, _), x, bindexp, cont) ->
      let bindv, heap'  = match bindexp with
        | Let v -> absval env v, heap
        | Op1 (op1, v) -> calc_op1 heap n op1 (absval env v)
        | Op2 (op2, v1, v2) -> calc_op2 heap op2 (absval env v1) (absval env v2)
        | Object fields ->
            let alloc_field (fname, fval) (absfields, h) = 
              let loc = LocField (n, fname) in
              (IdMap.add fname loc absfields, 
               Heap.add loc (absval env fval) h) in
            let absfields, h = fold_right alloc_field fields
              (IdMap.empty, heap) in
              (AVSet.singleton (AObj absfields), h)
              
        | UpdateField (obj, fname, fval) ->
            calc_op3 heap n (absval env obj) (absval env fname) 
              (absval env fval)
      in flow (IdMap.add x bindv env) heap' cont

and flow (env : env) (heap : heap) (cpsexp : cpsexp) : unit = 
  let idx = cpsexp_idx cpsexp in
    (* Reachability is a good debugging tool. *)
    if not (H.mem reachable idx) then
      H.add reachable idx cpsexp;

    let flow_env, env_updated =  try 
      begin
        let old_env = get_env idx in
        let new_env = union_env old_env env in
          if Pervasives.compare old_env new_env = 0 then
            (old_env, false)
            else 
              begin
                set_env idx new_env;
                (new_env, true)
              end
      end
    with Not_found -> (set_env idx env; env, true) in
    let flow_heap, heap_updated = try 
      begin
        let old_heap = H.find heaps idx in
        let new_heap = HeapExt.join AVSet.union old_heap heap in
          if Pervasives.compare old_heap new_heap = 0 then
            (old_heap, false)
          else
            begin
              H.replace heaps idx new_heap;
              (new_heap, true)
            end
      end
    with Not_found -> 
      H.add heaps idx heap;
      (heap, true) in
      if env_updated || heap_updated then
        calc flow_env flow_heap cpsexp
      

(* node is the Fix's node; new_env is the enclosing environment *)
and bind_lambda (n : int) (env : env) ((f, args, body) : lambda) = 
  IdMap.add f (AVSet.singleton (AClosure (cpsexp_idx body, args, body))) env

and mk_closure (env : env) ((f, args, body) : lambda) : unit = 
  let n = cpsexp_idx body in
    try
      ignore (get_env n)
    with
      | Not_found ->
          set_env n env
    

let cfa (cpsexp : cpsexp) : unit = flow IdMap.empty Heap.empty cpsexp

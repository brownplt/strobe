open FormatExt
open Prelude
open Lambdajs_cps
open Lambdajs_lattice
open AV
module H = Hashtbl

let reachable = H.create 100

let call_graph = H.create 500

let envs : (int, env) H.t = H.create 500

let heap : (int, AVSet.t) H.t = H.create 500

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

let calc_op1 (n : int) (op1 : op1) (avs : AV.avs) : avs = match op1 with
  | Ref -> 
      H.add heap n avs;
      AVSet.singleton (ARef n)
  | Deref ->
      let fn v r = match v with
        | ARef loc -> AVSet.union (H.find heap loc) r
        | _ -> r in
      let s = AVSet.fold fn avs AVSet.empty in
        s
  | Op1Prefix jsOp -> match jsOp with
      | JavaScript_syntax.PrefixLNot -> AVSet.singleton ANumber
      | JavaScript_syntax.PrefixBNot -> AVSet.singleton ABool
      | JavaScript_syntax.PrefixPlus -> AVSet.singleton ANumber
      | JavaScript_syntax.PrefixMinus -> AVSet.singleton ANumber
      | JavaScript_syntax.PrefixTypeof  -> AVSet.singleton AString
      | JavaScript_syntax.PrefixVoid ->
          AVSet.singleton (AConst Exprjs_syntax.CUndefined)

let rec get_fields obj_fields field_names : AVSet.t = match field_names with
  | [] -> AVSet.empty
  | field :: rest ->
        AVSet.union
          (try
             IdMap.find field obj_fields
           with Not_found ->
             AVSet.singleton (AConst Exprjs_syntax.CUndefined))
          (get_fields obj_fields rest)

let calc_op3 obj field_name field_value =
  if AVSet.mem AString field_name then
    failwith "set all fields"
  else 
    let upd_dict namev dict : avs IdMap.t = match namev with
      | AConst (Exprjs_syntax.CString s) -> IdMap.add s field_value dict
      | _ -> dict in
    let upd_obj objv result : avs  = match objv with
        AObj dict -> 
          AVSet.add (AObj (AVSet.fold upd_dict field_name dict)) result
      | _ -> result in
      AVSet.fold upd_obj obj AVSet.empty

let calc_op2 (op2 : op2) (vs1 : AV.avs) (vs2 : AV.avs) : avs = match op2 with
  | GetField ->
      if AVSet.mem AString vs2 then
        AVSet.fold
          (fun v r -> match v with
             | AObj props -> AVSetExt.unions (IdMapExt.values props)
             | _ -> r)
          vs1 AVSet.empty
      else 
        let fields_to_get = 
          AVSet.fold (fun v r -> match v with 
                        | AConst (Exprjs_syntax.CString s) -> IdSet.add s r
                        | _ ->  r) 
            vs2 IdSet.empty in
        let fields_to_get = IdSetExt.to_list fields_to_get in
          AVSet.fold (fun v r -> match v with
                        | AObj props ->
                            AVSet.union (get_fields props fields_to_get) r
                        | _ ->  r)
            vs1 AVSet.empty
  | SetRef -> 
      (* return previous value, vs2 *)
      let fn v = match v with
        | ARef loc ->
            let current_v = H.find heap loc in
              H.replace heap loc (AVSet.union current_v vs2)
        | _ -> () in
      AVSet.iter fn vs1;
      vs2
  | Op2Infix infixOp -> match infixOp with
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
      | JavaScript_syntax.OpAdd -> AVSet.singleton ANumber


let rec absval (env : AV.env) (cpsval : cpsval) : AV.avs = match cpsval with
  | Const c -> AVSet.singleton (AV.AConst c)
  | Array vs -> AVSet.singleton (AV.AArr (map (absval env) vs))
  | Object ps -> 
      AVSet.singleton 
        (AV.AObj (fold_left (fun m (x, v) -> IdMap.add x (absval env v) m)
                    IdMap.empty ps))
  | Id "#end" -> AVSet.empty
  | Id x -> 
      try IdMap.find x env
      with Not_found -> failwith ("unbound ads dentifier " ^ x)


let rec calc (env : env) (cpsexp : cpsexp) : unit = match cpsexp with
  | Fix (node, binds, body) ->
      let env' = fold_left (bind_lambda (fst2 node)) env binds in
        List.iter (mk_closure  env') binds;
        flow env' body
  | App ((app_n, _), f, args) ->
      let argvs = map (absval env) args in
      let do_app fv = match fv with 
        | AClosure (n, formals, body) -> (* TODO: arity *)
            call_from_to app_n (cpsexp_idx body);
            let body_env =
              List.fold_right2 IdMap.add formals argvs IdMap.empty in
             flow (union_env body_env (get_env n)) body
        | _ -> ()
      in AVSet.iter do_app (absval env f)

  | If (_, v1, e2, e3) -> 
      let av1 = absval env v1 in
        if AVSet.mem (AConst (Exprjs_syntax.CBool true)) av1 then
          flow env e2;
        if AVSet.mem (AConst (Exprjs_syntax.CBool false)) av1 then
          flow env e3
  | Bind ((n, _), x, bindexp, cont) ->
      let bindv = match bindexp with
        | Let v -> absval env v
        | Op1 (op1, v) -> calc_op1 n op1 (absval env v)
        | Op2 (op2, v1, v2) -> calc_op2 op2 (absval env v1) (absval env v2)
        | UpdateField (v1, v2, v3) ->
            calc_op3 (absval env v1) (absval env v2) (absval env v3)
      in flow (IdMap.add x bindv env) cont

and flow (env : env) (cpsexp : cpsexp) : unit = 
  let idx = cpsexp_idx cpsexp in
    if not (H.mem reachable idx) then
      H.add reachable idx cpsexp;
    try
      let old_env = get_env idx in
      let new_env = union_env old_env env in
        if compare_env old_env new_env = 0 then 
          ()
        else
          begin
            set_env idx new_env;
            calc new_env cpsexp
          end
    with Not_found ->
      set_env idx env;
      calc env cpsexp
      

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
    

let cfa (cpsexp : cpsexp) : unit = flow IdMap.empty cpsexp

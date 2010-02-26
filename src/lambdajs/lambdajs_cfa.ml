open FormatExt
open Prelude
open Lambdajs_cps
open Lambdajs_lattice
module H = Hashtbl

let reachable = H.create 100

let call_graph = H.create 500

let envs : (int, absenv) H.t = H.create 500

let heap : (int, absval) H.t = H.create 500

(* raises Not_found *)
let get_env (n : int) = Hashtbl.find envs n

let set_env (n : int) (env : absenv) = Hashtbl.replace envs n env

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

let calc_op1 (n : int) (op1 : op1) (avs : absval) : absval = match op1 with
  | Ref -> 
      H.add heap n avs;
      abs_ref n
  | Deref ->
      let fn av loc = abs_union (H.find heap loc) av in
        fold_left fn empty_absval (locs_of_val avs)
  | Op1Prefix jsOp -> match jsOp with
      | JavaScript_syntax.PrefixLNot -> any_num
      | JavaScript_syntax.PrefixBNot -> any_bool
      | JavaScript_syntax.PrefixPlus -> any_num
      | JavaScript_syntax.PrefixMinus -> any_num
      | JavaScript_syntax.PrefixTypeof  -> any_str
      | JavaScript_syntax.PrefixVoid -> abs_undef

let calc_op3 obj field_name field_value =
  if is_any_str field_name then
    failwith "set all fields"
  else 
    let fields = strings_of_val field_name in
    let objects = objs_of_val obj in
    let upd_field obj name = 
      upd_field name field_value obj in
    let upd_fields acc obj =
      abs_union (val_of_obj (fold_left upd_field obj fields)) acc in
      fold_left upd_fields empty_absval objects

let calc_op2 (op2 : op2) (vs1 : absval) (vs2 : absval) : absval = match op2 with
  | GetField ->
      if is_any_str vs2 then
        failwith "GET WITH ANY FIELD"
      else
        let objects = objs_of_val vs1 in
        let fields = strings_of_val vs2 in
        let get field acc obj =
          abs_union (get_field field obj) acc in
        let get_fields acc field =
          fold_left (get field) acc objects in
          fold_left get_fields empty_absval fields
  | SetRef -> 
      (* return previous value, vs2 *)
      let fn loc = 
        let current_v = H.find heap loc in
          H.replace heap loc (abs_union current_v vs2) in
        List.iter fn (locs_of_val vs1);
        vs2
  | Op2Infix infixOp -> match infixOp with
      | JavaScript_syntax.OpLT -> any_num
      | JavaScript_syntax.OpLEq  -> any_num
      | JavaScript_syntax.OpGT  -> any_num
      | JavaScript_syntax.OpGEq   -> any_num
      | JavaScript_syntax.OpIn -> any_bool
      | JavaScript_syntax.OpInstanceof -> any_bool
      | JavaScript_syntax.OpEq -> any_bool
      | JavaScript_syntax.OpNEq -> any_bool
      | JavaScript_syntax.OpStrictEq -> any_bool
      | JavaScript_syntax.OpStrictNEq -> any_bool
      | JavaScript_syntax.OpLAnd -> any_bool
      | JavaScript_syntax.OpLOr -> any_bool
      | JavaScript_syntax.OpMul -> any_num
      | JavaScript_syntax.OpDiv -> any_num
      | JavaScript_syntax.OpMod -> any_num
      | JavaScript_syntax.OpSub -> any_num
      | JavaScript_syntax.OpLShift -> any_num
      | JavaScript_syntax.OpSpRShift -> any_num
      | JavaScript_syntax.OpZfRShift -> any_num
      | JavaScript_syntax.OpBAnd -> any_num
      | JavaScript_syntax.OpBXor -> any_num
      | JavaScript_syntax.OpBOr -> any_num
      | JavaScript_syntax.OpAdd -> any_num


let rec absval (env : absenv) (cpsval : cpsval) : absval = match cpsval with
  | Const c -> abs_const c
  | Object ps -> 
      val_of_obj
        (mk_absobj
           (map (fun (f, v) -> (f, absval env v)) ps))
  | Id "#end" -> empty_absval
  | Id x -> 
      try lookup x env
      with Not_found -> failwith ("unbound identifier " ^ x)


let rec calc (env : absenv) (cpsexp : cpsexp) : unit = match cpsexp with
  | Fix (node, binds, body) ->
      let env' = fold_left (bind_lambda (fst2 node)) env binds in
        List.iter (mk_closure  env') binds;
        flow env' body
  | App ((app_n, _), f, args) ->
      let argvs = map (absval env) args in
      let do_app (n, formals, body) =(* TODO: arity *)
        call_from_to app_n (cpsexp_idx body);
        let body_env =
          List.fold_right2 bind formals argvs empty_absenv in
          flow (union_env body_env (get_env n)) body
      in List.iter do_app (closures_of_val (absval env f))

  | If (_, v1, e2, e3) -> 
      let av1 = absval env v1 in
        if is_true av1 then
          flow env e2;
        if is_false av1 then
          flow env e3
  | Bind ((n, _), x, bindexp, cont) ->
      let bindv = match bindexp with
        | Let v -> absval env v
        | Op1 (op1, v) -> calc_op1 n op1 (absval env v)
        | Op2 (op2, v1, v2) -> calc_op2 op2 (absval env v1) (absval env v2)
        | UpdateField (v1, v2, v3) ->
            calc_op3 (absval env v1) (absval env v2) (absval env v3)
      in flow (bind x bindv env) cont

and flow (env : absenv) (cpsexp : cpsexp) : unit = 
  let idx = cpsexp_idx cpsexp in
    if not (H.mem reachable idx) then
      H.add reachable idx cpsexp;
    try
      let old_env = get_env idx in
      let new_env = union_env old_env env in
        if eq_absenv old_env new_env then 
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
and bind_lambda (n : int) (env : absenv) ((f, args, body) : lambda) = 
  bind f (closure args body) env

and mk_closure (env : absenv) ((f, args, body) : lambda) : unit = 
  let n = cpsexp_idx body in
    try
      ignore (get_env n)
    with
      | Not_found ->
          set_env n env
    

let cfa (cpsexp : cpsexp) : unit = flow empty_absenv cpsexp

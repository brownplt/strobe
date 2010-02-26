open FormatExt
open Prelude
open Lambdajs_cps
module H = Hashtbl

module rec AV 
  : sig
    type t = 
      | ANumber
      | ABool
      | AString
      | AConst of Exprjs_syntax.const
      | ARef of int
      | AObj of (string * avs) list
      | AArr of avs list
      | AClosure of int * id list * cpsexp

    and avs = AVSet.t
    and env = avs IdMap.t
    val compare : t -> t -> int
    val compare_env : env -> env -> int
  end = 
struct
  type t = 
    | ANumber
    | ABool
    | AString
    | AConst of Exprjs_syntax.const
    | ARef of int
    | AObj of (string * avs) list
    | AArr of avs list
    | AClosure of int * id list * cpsexp
  and avs = AVSet.t
  and env = avs IdMap.t
      
  let compare = Pervasives.compare 

  let compare_env env1 env2 = 
    IdMap.compare AVSet.compare env1 env2
end
  
and AVSet 
  : Set.S with type elt = AV.t 
  = Set.Make (AV)

module AVSetExt = SetExt.Make (AVSet)

open AV


let rec  p_av av fmt = match av with
  | AConst c -> Exprjs_pretty.p_const c fmt
  | ARef x -> Format.pp_print_int  fmt x
  | AObj lst -> Format.pp_print_string fmt "obj"
  | AArr _ -> Format.pp_print_string fmt  "arr"
  | AClosure (n, args, _) -> 

Format.pp_print_string fmt  ("closure" ^ string_of_int n)
      

let union_env (env1 : AV.env) (env2 : AV.env) : AV.env = 
  IdMapExt.join AVSet.union  env1 env2


let envs : (int, env) H.t = H.create 500

let next_loc = ref 0

let heap : (int, AVSet.t) H.t = H.create 500



(* raises Not_found *)
let get_env (n : int) = Hashtbl.find envs n

let set_env (n : int) (env : env) = Hashtbl.replace envs n env

open Lambdajs_syntax

let calc_op1 (op1 : op1) (avs : AV.avs) : avs = match op1 with
  | Ref -> 
      let loc = !next_loc in
        incr next_loc;
        H.add heap loc avs;
        AVSet.singleton (ARef loc)
  | Deref ->
      let fn v r = match v with
        | ARef loc -> AVSet.union (H.find heap loc) r
        | _ -> r in
      AVSet.fold fn avs AVSet.empty
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
             List.assoc field obj_fields
           with Not_found -> AVSet.singleton (AConst Exprjs_syntax.CUndefined))
          (get_fields obj_fields rest)

let calc_op3 obj field_name field_value =
  obj

let calc_op2 (op2 : op2) (vs1 : AV.avs) (vs2 : AV.avs) : avs = match op2 with
  | GetField ->
      if AVSet.mem AString vs2 then
        AVSet.fold
          (fun v r -> match v with
             | AObj props -> 
                 AVSetExt.unions (r :: (map snd2 props))
             | _ -> r)
          vs1 AVSet.empty
      else 
        let fields_to_get = 
          AVSet.fold (fun v r -> match v with 
                        | AConst (Exprjs_syntax.CString s) -> IdSet.add s r
                        | _ -> r) 
            vs2 IdSet.empty in
        let fields_to_get = IdSetExt.to_list fields_to_get in
          AVSet.fold (fun v r -> match v with
                        | AObj props -> 
                            AVSet.union (get_fields props fields_to_get) r
                        | _ -> r)
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
      AVSet.singleton (AV.AObj (map (fun (x, v) -> (x, absval env v)) ps))
  | Id "#end" -> AVSet.empty
  | Id x -> 
      try IdMap.find x env
      with Not_found -> failwith ("unbound ads dentifier " ^ x)


let rec calc (env : env) (cpsexp : cpsexp) : unit = match cpsexp with
  | Fix (node, binds, body) ->
      let env' = fold_left (bind_lambda (fst2 node)) env binds in
        List.iter (mk_closure  env') binds;
        flow env' body
  | App (_, f, args) ->
      let argvs = map (absval env) args in
      let do_app fv = match fv with 
        | AClosure (n, formals, body) -> (* TODO: arity *)
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
  | Let0 (_, x, v, e) ->
      flow (IdMap.add x (absval env v) env) e
  | Let1 (_, x, op1, v, e) ->
      flow (IdMap.add x (calc_op1 op1 (absval env v)) env) e
  | Let2 (_, x, op2, v1, v2, e) ->
      flow (IdMap.add x (calc_op2 op2 (absval env v1) (absval env v2)) env) e
  | UpdateField (_, x, v1, v2, v3, e) ->
      flow (IdMap.add x 
              (calc_op3 (absval env v1) (absval env v2) (absval env v1)) env) e



and flow (env : env) (cpsexp : cpsexp) : unit = 
  try
    let old_env = get_env (cpsexp_idx cpsexp) in
    let new_env = union_env old_env env in
    if compare_env old_env new_env = 0 then 
      ()
    else
      begin
        set_env (cpsexp_idx cpsexp) new_env;
        calc new_env cpsexp
      end
  with Not_found ->
    set_env (cpsexp_idx cpsexp) env;
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

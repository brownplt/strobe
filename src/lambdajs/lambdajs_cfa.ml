open FormatExt
open Prelude
open Lambdajs_cps
module H = Hashtbl

module rec AV 
  : sig
    type t = 
      | AConst of Exprjs_syntax.const
      | ARef of id
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
    | AConst of Exprjs_syntax.const
    | ARef of id
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
  | ARef x -> Format.pp_print_string fmt x
  | AObj lst -> Format.pp_print_string fmt "obj"
  | AArr _ -> Format.pp_print_string fmt  "arr"
  | AClosure (n, args, _) -> 

Format.pp_print_string fmt  ("closure" ^ string_of_int n)
      

let union_env (env1 : AV.env) (env2 : AV.env) : AV.env = 
  IdMapExt.join AVSet.union  env1 env2




let envs : (int, env) H.t = H.create 500

(* raises Not_found *)
let get_env (n : int) = Hashtbl.find envs n

let set_env (n : int) (env : env) = Hashtbl.replace envs n env


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

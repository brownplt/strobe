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

let make_ATypeIs x s = match s with
    "string" -> Type.LocTypeIs (x, RTSet.singleton RT.String)
  | "number" -> Type.LocTypeIs (x, RTSet.singleton RT.Number)
  | "boolean" -> Type.LocTypeIs (x, RTSet.singleton RT.Boolean)
  | "function" -> Type.LocTypeIs (x, RTSet.singleton RT.Function)
  | "object" -> Type.LocTypeIs (x, RTSet.singleton RT.Object)
  | "undefined" -> Type.LocTypeIs (x, RTSet.singleton RT.Undefined)
  | _ -> singleton ABool

let is_string v = match v with
  | AString -> true
  | AConst (Exprjs_syntax.CString _) -> true
  | _ -> false

let is_number v = match v with
  | ANumber -> true
  | AConst (Exprjs_syntax.CInt _) -> true
  | AConst (Exprjs_syntax.CNum _) -> true
  | _ -> false


let restrict_filter rt t = match t with
  | AV.ANumber -> RTSet.mem RT.Number rt
  | AV.ABool -> RTSet.mem RT.Boolean rt
  | AV.AString -> RTSet.mem RT.String rt
  | AV.AConst c -> begin match c with
      | Exprjs_syntax.CString _ -> RTSet.mem RT.String rt
      | Exprjs_syntax.CRegexp _ -> RTSet.mem RT.Object rt
      | Exprjs_syntax.CNum _ -> RTSet.mem RT.Number rt
      | Exprjs_syntax.CInt _ -> RTSet.mem RT.Number rt
      | Exprjs_syntax.CBool _ -> RTSet.mem RT.Boolean rt
      | Exprjs_syntax.CNull -> RTSet.mem RT.Object rt
      | Exprjs_syntax.CUndefined -> RTSet.mem RT.Undefined rt
    end 
  | ARef _ -> false (* TODO: what?? *)
  | AObj _ -> RTSet.mem RT.Object rt
  | AClosure _ -> RTSet.mem RT.Function rt

let remove_filter rt t = not (restrict_filter rt t)


let restrict_loc (h : heap) (l : loc) (r : RTSet.t) = 
  let set = Range.up (deref l h) in
  let set' = AVSet.filter (restrict_filter r) set in
    if AVSet.is_empty set' then None
    else Some (set_ref l (Range.Set set') h)

let remove_loc (h : heap) (l : loc) (r : RTSet.t) =
  let set = Range.up (deref l h) in
  let set' = AVSet.filter (remove_filter r) set in
    if AVSet.is_empty set' then None
    else Some (set_ref l (Range.Set set') h)

open Lambdajs_syntax

let calc_op1 h (n : int) (op1 : op1) (avs : Type.t) = match op1 with
  | Ref -> 
      let loc = Loc n in
      singleton (ARef loc), set_ref loc (Type.up h avs) h
  | Deref -> begin match Range.up (Type.up h avs) with
      | set when AVSet.cardinal set = 1 ->
          begin match AVSet.choose set with
            | ARef l -> Type.Deref l, h
            | _ -> empty, h (* type error *)
          end
      | avs ->
          let fn v r = match v with
            | ARef loc -> AVSet.union (Range.up (deref loc h)) r
            | _ -> r in
            Type.Range (Range.Set (AVSet.fold fn avs AVSet.empty)), h
    end
  | Op1Prefix jsOp -> 
      let r = match jsOp with
        | JavaScript_syntax.PrefixLNot -> singleton ANumber
        | JavaScript_syntax.PrefixBNot -> singleton ABool
        | JavaScript_syntax.PrefixPlus -> singleton ANumber
        | JavaScript_syntax.PrefixMinus -> singleton ANumber
        | JavaScript_syntax.PrefixTypeof -> begin match avs with
            | Type.Deref l -> Type.LocTypeof l
            | _ -> singleton AString
          end
        | JavaScript_syntax.PrefixVoid ->
            singleton (AConst Exprjs_syntax.CUndefined) in
        r, h

let rec get_fields h obj_fields field_names : Type.t = match field_names with
  | [] -> empty
  | field :: rest ->
      if not (IdMap.mem field obj_fields) then
        Type.union h (singleton (AConst Exprjs_syntax.CUndefined))
          (get_fields h obj_fields rest)
      else 
        let field_loc = IdMap.find field obj_fields in
        let field_val = Type.Range (Range.Set (Range.up (deref field_loc h))) in
          Type.union h field_val  (get_fields h obj_fields rest)


let calc_op2 node h op2 (v1 : Type.t) (v2 : Type.t) = match op2 with
  | GetField -> 
      let vs1 = Range.up (Type.up h v1) in
      let vs2 = Range.up (Type.up h v2) in
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
                              Type.union h (get_fields h props fields_to_get) r
                          | _ ->  r)
              vs1 empty, h
  | SetRef -> 
      let fn v h = match v with
        | ARef loc -> set_ref loc (Type.up h v2) h
        | _ -> eprintf "%d: SetRef on %s\n" node (to_string AV.pp v);
            h in
        v2, AVSet.fold fn (Range.up (Type.up h v1)) h
  | Prim2 "+" ->
      begin match Type.up h v1, Type.up h v2 with
        | r1, r2 ->
            printf "LHS is %s, RHS is %s\n" (to_string Range.pp r1)
              (to_string Range.pp r2);
            singleton ANumber
      end, h

  | Op2Infix JavaScript_syntax.OpStrictEq ->
      begin match v1, v2 with
        | Type.LocTypeof x, Type.Range (Range.Set set) ->
            if AVSet.cardinal set = 1 then
              match AVSet.choose set with
                | AConst (Exprjs_syntax.CString s) -> make_ATypeIs x s
                | _ -> singleton ABool
            else
              singleton ABool
        | _ -> singleton ABool
      end, h
            
  | Op2Infix infixOp -> 
      (match infixOp with
         | JavaScript_syntax.OpLT ->
             begin match v1, Type.up h v2 with
               | Type.Deref l, Range.Range (lb, ub) ->
                   Type.LocRangeIs (l, Range.Range (Range.NegInf,ub))
               | _ -> singleton ABool
             end
      | JavaScript_syntax.OpLEq  -> singleton ABool
      | JavaScript_syntax.OpGT  -> singleton ABool
      | JavaScript_syntax.OpGEq   -> singleton ABool
      | JavaScript_syntax.OpIn -> singleton ABool
      | JavaScript_syntax.OpInstanceof -> singleton ABool
      | JavaScript_syntax.OpEq -> singleton ABool
      | JavaScript_syntax.OpNEq -> singleton ABool
      | JavaScript_syntax.OpStrictEq -> singleton ABool
      | JavaScript_syntax.OpStrictNEq -> singleton ABool
      | JavaScript_syntax.OpLAnd -> singleton ABool
      | JavaScript_syntax.OpLOr -> singleton ABool
      | JavaScript_syntax.OpMul -> singleton ANumber
      | JavaScript_syntax.OpDiv -> singleton ANumber
      | JavaScript_syntax.OpMod -> singleton ANumber
      | JavaScript_syntax.OpSub -> singleton ANumber
      | JavaScript_syntax.OpLShift -> singleton ANumber
      | JavaScript_syntax.OpSpRShift -> singleton ANumber
      | JavaScript_syntax.OpZfRShift -> singleton ANumber
      | JavaScript_syntax.OpBAnd -> singleton ANumber
      | JavaScript_syntax.OpBXor -> singleton ANumber
      | JavaScript_syntax.OpBOr -> singleton ANumber
      | JavaScript_syntax.OpAdd -> 
          begin match Type.up h v1, Type.up h v2 with
            | Range.Range (lb1, ub1), Range.Range (lb2, ub2) ->
                let lb, ub = Range.add (lb1, ub1) (lb2, ub2) in
                  Type.Range (Range.Range (lb, ub))
            | r1, r2 ->
                printf "LHS is %s, RHS is %s\n" (to_string Range.pp r1)
                  (to_string Range.pp r2);
                singleton ANumber
          end), h
        
let obj_values h obj = match obj with
  | AObj locs -> 
      IdMap.fold
        (fun fname floc dict -> IdMap.add fname (Range.up (deref floc h)) dict)
        locs IdMap.empty
  | _ -> IdMap.empty

let rec calc_op3 h n obj field_name field_value = 
  let obj = Range.up (Type.up h obj) in
  let field_name = Range.up (Type.up h field_name) in
    if AVSet.mem AString field_name then
      failwith "set all fields"
    else 
      (* Map from field name to value-sets. This maps the fields of all
         objects to all their values. *)
      let dict = AVSet.fold 
        (fun obj dict -> IdMapExt.join AVSet.union (obj_values h obj) dict)
        obj IdMap.empty in
      let dict = AVSet.fold
        (fun fname dict -> match fname with
           | AConst (Exprjs_syntax.CString s) ->
             IdMap.add s field_value  dict
           | _ -> dict )
        field_name dict in
      let alloc_field fname fval (absfields, h) = 
        let loc = LocField (n, fname) in
        (IdMap.add fname loc absfields, set_ref loc (Range.Set fval) h) in
      let absfields, h = IdMap.fold alloc_field dict (IdMap.empty, h) in
        (singleton (AObj absfields), h)
          

let rec absval (env : env) (cpsval : cpsval) : Type.t = match cpsval with
  | Const (Exprjs_syntax.CInt n) -> 
      Type.Range (Range.Range (Range.Int n, Range.Int n))
  | Const c -> singleton (AV.AConst c)
  | Id "#end" -> empty
  | Id x -> lookup x env


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
              List.fold_right2 bind formals argvs empty_env in
              flow (union_env heap body_env (get_env n)) heap body
        | _ -> eprintf "Failed application at %d.\n" app_n in
        AVSet.iter do_app (Range.up (Type.up heap (absval env f)))
  | If ((n, _), v1, e2, e3) -> 
      begin match absval env v1 with
        | Type.Range (Range.Set set) ->
            if AVSet.mem (AConst (Exprjs_syntax.CBool true)) set 
              || AVSet.mem ABool set then
              flow env heap e2;
            if AVSet.mem (AConst (Exprjs_syntax.CBool false)) set 
              || AVSet.mem ABool set then
              flow env heap e3
        | Type.LocRangeIs (loc, r1) ->
            let curr = deref loc heap in
              begin match curr with
              | Range.Range _ -> 
                  let new_range = Range.intersect curr r1 in
                    flow env (set_ref loc new_range heap) e2;
                    flow env heap e3
              | _ -> flow env heap e2; flow env heap e3
              end 
        | Type.LocTypeIs (l, rt) ->
            begin match restrict_loc heap l rt with
              | None -> ()
              | Some true_heap -> flow env true_heap e2
            end;
            begin match remove_loc heap l rt with
              | None -> ()
              | Some false_heap -> flow env false_heap e3
            end
        | _ -> eprintf "Non-boolean test value at %d.\n" n
      end
  | Bind ((n, _), x, bindexp, cont) ->
      let bindv, heap'  = match bindexp with
        | Let v -> absval env v, heap
        | Op1 (op1, v) -> calc_op1 heap n op1 (absval env v)
        | Op2 (op2, v1, v2) -> 
            calc_op2 n heap op2 (absval env v1) (absval env v2)
        | Object fields ->
            let alloc_field (fname, fval) (absfields, h) = 
              let loc = LocField (n, fname) in
              (IdMap.add fname loc absfields, 
               set_ref loc (Type.up  h (absval env fval)) h) in
            let absfields, h = fold_right alloc_field fields
              (IdMap.empty, heap) in
              (singleton (AObj absfields), h)
              
        | UpdateField (obj, fname, fval) ->
            calc_op3 heap n (absval env obj) (absval env fname) 
              (Range.up (Type.up heap (absval env fval)))
      in flow (bind x bindv env) heap' cont

and flow (env : env) (heap : heap) (cpsexp : cpsexp) : unit = 
  let idx = cpsexp_idx cpsexp in
    (* Reachability is a good debugging tool. *)
    if not (H.mem reachable idx) then
      H.add reachable idx cpsexp;

    let flow_heap, reflow_heap =
      try
        begin
          let old_heap = H.find heaps idx in
          let new_heap = union_heap old_heap heap in
            if compare_heap old_heap new_heap = 0 then
              (old_heap, false)
            else
              begin
                H.replace heaps idx new_heap;
                (new_heap, true)
              end
        end
      with Not_found -> 
        (H.add heaps idx heap;
         (heap, true))
    in let flow_env, reflow_env =
      try 
        begin
          let old_env = get_env idx in
          let new_env = union_env flow_heap old_env env in
            if compare_env old_env new_env = 0 then
              (old_env, false)
            else 
              begin
                set_env idx new_env;
                (new_env, true)
              end
        end
      with Not_found ->
        (set_env idx env;
         env, true)
    in if reflow_heap || reflow_env then
        begin
          calc flow_env flow_heap cpsexp
        end

(* node is the Fix's node; new_env is the enclosing environment *)
and bind_lambda (n : int) (env : env) ((f, args, body) : lambda) = 
  bind f (singleton (AClosure (cpsexp_idx body, args, body))) env

and mk_closure (env : env) ((f, args, body) : lambda) : unit = 
  let n = cpsexp_idx body in
    try
      ignore (get_env n)
    with
      | Not_found ->
          set_env n env
    

let cfa (cpsexp : cpsexp) : unit = flow empty_env empty_heap cpsexp

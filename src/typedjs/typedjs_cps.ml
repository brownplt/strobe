open Prelude
open Typedjs_syntax 

type cpsval =
    Const of const
  | Id of id
  | Array of cpsval list
  | Object of (id * cpsval) list

type node = int

type cpsexp =
    Fix of node * (id * id list * typ * cpsexp) list * cpsexp
  | App of node * cpsval * cpsval list
  | If of node * cpsval * cpsexp * cpsexp
  | Let0 of node * id * cpsval * cpsexp (* load immediate / reg-reg move *)
  | Let1 of node * id * JavaScript_syntax.prefixOp * cpsval * cpsexp
  | Let2 of node * id * JavaScript_syntax.infixOp * cpsval * cpsval * cpsexp
  | Assign of node * id * cpsval * cpsexp
  | SetProp of node * cpsval * cpsval * cpsval * cpsexp

let new_name : unit -> id = 
  let next_name = ref 0 in
    fun () ->
      incr next_name;
      "%anf" ^ string_of_int (!next_name - 1)

let new_node : unit -> node =
  let next_node = ref 0 in
    fun () -> 
      incr next_node;
      !next_node - 1

type cont = cpsval -> cpsexp

let rec cps_exp (exp : exp) (k : cont) : cpsexp = match exp with
    EConst (_, c) -> k (Const c)
  | EArray (_, es) -> cps_exp_list es (fun vs -> k (Array vs))
  | EId (_, x) -> k (Id x)
  | EObject (_, ps) ->
      cps_exp_list (map thd3 ps) 
        (fun vs -> (k (Object (List.combine (map fst3 ps) vs))))
  | EPrefixOp (_, op, e) ->
      cps_exp e 
        (fun v -> 
           let x = new_name () in
             Let1 (new_node (), x, op, v, k (Id x)))
  | EInfixOp (_, op, e1, e2) -> 
      cps_exp e1 
        (fun v1 ->
           cps_exp e2 
             (fun v2 ->
                let x = new_name () in
                  Let2 (new_node (), x, op, v1, v2, k (Id x))))
  | EIf (_, e1, e2, e3) -> 
      cps_exp e1
        (fun v1 ->
           let k' = new_name ()
           and r = new_name () in
             Fix (new_node (), 
                  [k', [r], TTop, k (Id r)],
                  If (new_node (),
                      v1, 
                      cps_exp e2 (fun v -> App (new_node (), Id k', [v])),
                      cps_exp e3 (fun v -> App (new_node (), Id k', [v])))))
  | EAssign (_, LVar (_, x), e) -> 
      cps_exp e (fun v -> Assign (new_node (), x, v, k v))
  | EAssign (_, LProp (_, e1, e2), e3) ->
      cps_exp e1
        (fun v1 ->
           cps_exp e2
             (fun v2 ->
                cps_exp e3
                  (fun v3 ->
                     SetProp (new_node (), v1, v2, v3, k v3))))
  | EApp (_, func, args) -> 
      let k' = new_name ()
      and r = new_name () in
        Fix (new_node (),
             [(k', [r], TTop, k (Id r))],
             cps_exp func
               (fun f ->
                  cps_exp_list args 
                    (fun vs -> App (new_node (), f, (Id k') :: vs))))
  | EFunc (_, args, typ, body) -> 
      let f = new_name ()
      and k' = new_name () in
        Fix (new_node (),
             [(f, k' :: args, typ, cps_exp body 
                 (fun v -> App (new_node (), Id k', [v])))],
             k (Id f))
  | ELet (_, x, e1, e2) ->
      cps_exp e1
        (fun v1 ->
           Let0 (new_node (), x, v1, cps_exp e2 k))
  | ERec (binds, body) ->
      Fix (new_node (), map cps_bind binds, cps_exp body k)
  | ESeq (_, e1, e2) ->
      cps_exp e1
        (fun v -> Let0 (new_node (), new_name (), v, cps_exp e2 k))
  | ELabel (_, l, _, e) ->
      let r = new_name () in
        Fix (new_node (),
             [(l, [r], TTop, k (Id r))],
             cps_exp e (fun v -> App (new_node (), Id l, [v])))
  | EBreak (_, l, e) -> (* drops its own continuation *)
      cps_exp e (fun v -> App (new_node (), Id l, [v]))

(*
  | ETryCatch of pos * exp * id * exp
  | ETryFinally of pos * exp * exp
  | EThrow of pos * exp
  | ETypecast of pos * runtime_typs * exp *)



and cps_bind ((name, typ, e) : id * typ * exp) = match e with
    EFunc (_, args, _, body) ->
      let k = new_name () in
        (name, k :: args, typ, cps_exp body 
           (fun v -> (App (new_node (), Id k, [v]))))
  | _ -> failwith "cps_bind : expected a function"
  

and cps_exp_list (exps : exp list) (k : cpsval list -> cpsexp) = match exps with
    [] -> k []
  | e :: rest ->
      cps_exp e 
        (fun v ->
           cps_exp_list rest
             (fun vs ->
                k (v :: vs)))

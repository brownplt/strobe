open Prelude
open Typedjs_syntax 

type cpsval =
    Const of const
  | Id of id
  | Array of cpsval list (* array and obj should be named *)
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
      "%cps" ^ string_of_int (!next_name - 1)

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
             cps_tailexp e l)
  | EBreak (_, l, e) -> (* drops its own continuation *)
      cps_tailexp e l

(*
  | ETryCatch of pos * exp * id * exp
  | ETryFinally of pos * exp * exp
  | EThrow of pos * exp
  | ETypecast of pos * runtime_typs * exp *)

and cps_tailexp (exp : exp) (k : id) : cpsexp = match exp with
    EConst (_, c) -> App (new_node (), Id k, [ Const c ])
  | EArray (_, es) ->
      cps_exp_list es (fun vs -> App (new_node (), Id k, [ Array vs ]))
  | EId (_, x) -> App (new_node (), Id k, [ Id x ])
  | EObject (_, ps) ->
      cps_exp_list (map thd3 ps) 
        (fun vs -> 
           App (new_node (), Id k, [ Object (List.combine (map fst3 ps) vs) ]))
  | EPrefixOp (_, op, e) ->
      cps_exp e 
        (fun v -> 
           let x = new_name () in
             Let1 (new_node (), x, op, v, 
                   App (new_node (), Id k, [ Id x ])))
  | EInfixOp (_, op, e1, e2) -> 
      cps_exp e1 
        (fun v1 ->
           cps_exp e2 
             (fun v2 ->
                let x = new_name () in
                  Let2 (new_node (), x, op, v1, v2, 
                        App (new_node (), Id k, [ Id x ]))))
  | EIf (_, e1, e2, e3) -> 
      cps_exp e1
        (fun v1 ->
           If (new_node (),
               v1, 
               cps_tailexp e2 k, 
               cps_tailexp e3 k))
  | EAssign (_, LVar (_, x), e) -> 
      cps_exp e (fun v -> 
                   Assign (new_node (), x, v,
                           App (new_node (), Id k, [ v ])))
  | EAssign (_, LProp (_, e1, e2), e3) ->
      cps_exp e1
        (fun v1 ->
           cps_exp e2
             (fun v2 ->
                cps_exp e3
                  (fun v3 ->
                     SetProp (new_node (), v1, v2, v3,
                              App (new_node (), Id k, [ v3 ])))))
  | EApp (_, func, args) -> 
      cps_exp func
        (fun f ->
           cps_exp_list args 
             (fun vs -> App (new_node (), f, Id k :: vs)))
  | EFunc (_, args, typ, body) -> 
      let f = new_name ()
      and k' = new_name () in
        Fix (new_node (),
             [(f, k' :: args, typ, cps_tailexp body k')],
             App (new_node (), Id k, [ Id f ]))
  | ELet (_, x, e1, e2) ->
      cps_exp e1
        (fun v1 ->
           Let0 (new_node (), x, v1, cps_tailexp e2 k))
  | ERec (binds, body) ->
      Fix (new_node (), map cps_bind binds, cps_tailexp body k)
  | ESeq (_, e1, e2) ->
      cps_exp e1
        (fun v -> Let0 (new_node (), new_name (), v, cps_tailexp e2 k))
  | ELabel (_, l, _, e) ->
      let r = new_name () in
        Fix (new_node (),
             [(l, [r], TTop, App (new_node (), Id k, [Id r]))],
             cps_tailexp e k)
  | EBreak (_, l, e) -> (* drops its own continuation *)
      cps_tailexp e l


and cps_bind ((name, typ, e) : id * typ * exp) = match e with
    EFunc (_, args, _, body) ->
      let k = new_name () in
        (name, k :: args, typ, cps_tailexp body k)
  | _ -> failwith "cps_bind : expected a function"
  

and cps_exp_list (exps : exp list) (k : cpsval list -> cpsexp) = match exps with
    [] -> k []
  | e :: rest ->
      cps_exp e 
        (fun v ->
           cps_exp_list rest
             (fun vs ->
                k (v :: vs)))

let cps (exp : exp) : cpsexp = cps_tailexp exp "%end"


(******************************************************************************)

open Format
open FormatExt

let rec p_cpsval (cpsval : cpsval) : printer = match cpsval with
    Const c -> Typedjs_pretty.p_const c
  | Id x -> text x
  | Array vs -> parens (text "array" :: (map p_cpsval vs))
  | Object ps -> parens (text "object" :: (map p_prop ps))
    
and p_prop (x, v) : printer = brackets [ text x; p_cpsval v ]

let rec p_cpsexp (cpsexp : cpsexp) : printer = match cpsexp with
    Fix (_, binds, body) ->
      parens [ text "fix"; parens (map p_bind binds); p_cpsexp body ]
  | App (_, f, args ) ->
      parens ( text "app" :: p_cpsval f :: (map p_cpsval args) )
  | If (_, v1, e2, e3) -> 
      parens [ text "if"; p_cpsval v1; p_cpsexp e2; p_cpsexp e3 ]
  | Let0 (_, x, v, e) -> 
      parens [ text "let"; parens [ text x; p_cpsval v ]; p_cpsexp e ]
  | Let1 (_, x, op, v, e) -> 
      parens [ text "let";
               parens [ text x; 
                        parens [ text (JavaScript_pretty.render_prefixOp op);
                                 p_cpsval v ] ];
               p_cpsexp e ]
  | Let2 (_, x, op, v1, v2, e) -> 
      parens [ text "let";
               parens [ text x; 
                        parens [ text (JavaScript_pretty.render_infixOp op);
                                 p_cpsval v1;
                                 p_cpsval v2 ] ];
               p_cpsexp e ]
  | Assign (_, x, v, e) -> 
      parens [ text "seq";
               parens [ text "set!"; text x; p_cpsval v ];
               p_cpsexp e ]
  | SetProp (_, v1, v2, v3, e) -> 
      parens [ text "seq";
               parens (text "set-field!" :: (map p_cpsval [ v1; v2; v3 ]));
               p_cpsexp e ]

and p_bind (f, args, typ, body) : printer =
    brackets [ text f; 
               parens [ text "lambda"; parens (map text args); p_cpsexp body ] ]

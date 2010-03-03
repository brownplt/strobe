open Prelude
open Typedjs_syntax 

type cpsval =
    Const of Exprjs_syntax.const
  | Id of id

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
  | Array of node * id * cpsval list * cpsexp
  | Object of node * id * (string * cpsval) list * cpsexp
  | GetField of node * id * cpsval * cpsval * cpsexp

let mk_name : string -> id = 
  let next_name = ref 0 in
    fun str ->
      incr next_name;
      "%" ^ str ^ string_of_int (!next_name - 1)

let new_name () = mk_name "cps"

let new_node : unit -> node =
  let next_node = ref 0 in
    fun () -> 
      incr next_node;
      !next_node - 1

type cont = cpsval -> cpsexp

let rec cps_exp  (exp : exp) (k : cont) : cpsexp = match exp with
    EConst (_, c) -> k (Const c)
  | EId (_, x) -> k (Id x)
  | EArray (_, es) -> 
      cps_exp_list es 
        (fun vs ->
           let x = new_name () in
             Array (new_node (), x, vs, k (Id x)))
  | EObject (_, ps) ->
      cps_exp_list (map thd3 ps) 
        (fun vs -> 
           let x = new_name () in
             Object (new_node (), x,
                     List.combine (map (fun (f, _, _) -> f) ps) vs,
                     k (Id x)))
  | EBracket (_, e1, e2) ->
      cps_exp e1
        (fun v1 ->
           cps_exp e2
             (fun v2 ->
                let x = new_name () in
                  GetField (new_node (), x, v1, v2, k (Id x))))
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
           let k' = mk_name "if-cont"
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
      let k' = mk_name "app-cont"
      and r = new_name () in
        Fix (new_node (),
             [(k', [r], TTop, k (Id r))],
             cps_exp func
               (fun f ->
                  cps_exp_list args 
                    (fun vs -> App (new_node (), f, (Id k') :: vs))))
  | EFunc (_, args, typ, body) -> 
      let f = mk_name "anon-func"
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
  | EId (_, x) -> App (new_node (), Id k, [ Id x ])
  | EArray (_, es) -> 
      cps_exp_list es 
        (fun vs ->
           let x = new_name () in
             Array (new_node (), x, vs, App (new_node (), Id k, [ Id x ])))
  | EObject (_, ps) ->
      cps_exp_list (map thd3 ps) 
        (fun vs -> 
           let x = new_name () in
             Object (new_node (), x,
                     List.combine
                       (map (fun (f, _, _) -> f) ps) vs,
                     App (new_node (), Id k, [ Id x ])))
  | EBracket (_, e1, e2) ->
      cps_exp e1
        (fun v1 ->
           cps_exp e2
             (fun v2 ->
                let x = new_name () in
                  GetField (new_node (), x, v1, v2,
                            App (new_node (), Id k, [ Id x ]))))
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

let node_of_cpsexp (cpsexp : cpsexp) : node = match cpsexp with
    Fix (n, _, _) -> n
  | App (n, _, _) -> n
  | If (n, _, _, _) -> n
  | Let0 (n, _, _, _) -> n
  | Let1 (n, _, _, _, _) -> n
  | Let2 (n, _, _, _, _, _) -> n
  | Assign (n, _, _, _) -> n
  | SetProp (n, _, _, _, _) -> n
  | GetField (n, _, _, _, _) -> n
  | Object (n, _, _, _) -> n
  | Array (n, _, _, _) -> n

(******************************************************************************)

let fv_cpsval (cpsval : cpsval) : IdSet.t = match cpsval with
    Const _ -> IdSet.empty
  | Id x -> IdSet.singleton x

let fv_prop (_, v) = fv_cpsval v

let rec esc_cpsexp (cpsexp : cpsexp) : IdSet.t = match cpsexp with
    Fix (_, binds, body) ->
      IdSetExt.unions (esc_cpsexp body :: (map esc_bind binds))
  | App (_, _, vs) ->
      (* An identifier in function position does not "escape," even if it is
         a call to a known function. *)
      IdSetExt.unions (map fv_cpsval vs)
  | If (_, _, e1, e2) -> IdSet.union (esc_cpsexp e1) (esc_cpsexp e2)
  | Let0 (_, x, _, e) -> IdSet.remove x (esc_cpsexp e)
  | Let1 (_, x, _, _, e) -> IdSet.remove x (esc_cpsexp e)
  | Let2 (_, x, _, _, _, e) -> IdSet.remove x (esc_cpsexp e)
  | Assign (_, _, _, e) -> esc_cpsexp e
  | SetProp (_, _, _, _, e) -> esc_cpsexp e
  | Array (_, x, vs, e) -> 
      IdSet.union
        (IdSetExt.unions (map fv_cpsval vs))
        (IdSet.remove x (esc_cpsexp e))
  | Object (_, x, props, e) -> 
      IdSet.union
        (IdSetExt.unions (map fv_prop props))
        (IdSet.remove x (esc_cpsexp e))
  | GetField (_, x, _, _, e) ->
      IdSet.remove x (esc_cpsexp e)

and esc_bind (_, args, _, e) = 
  IdSet.diff (esc_cpsexp e) (IdSetExt.from_list args)



(******************************************************************************)

open Format
open FormatExt

let rec p_cpsval (cpsval : cpsval) : printer = match cpsval with
    Const c -> Exprjs_pretty.p_const c
  | Id x -> text x
    
and p_prop (x, v) : printer = brackets [ text x; p_cpsval v ]

let rec p_cpsexp (cpsexp : cpsexp) : printer = match cpsexp with
    Fix (_, binds, body) ->
      vert [ text "fix"; nest (vert (map p_bind binds)); p_cpsexp body ]

  | App (_, f, args ) ->
      parens ( text "app" :: p_cpsval f :: (map p_cpsval args) )
  | If (_, v1, e2, e3) -> 
      parens [ text "if"; p_cpsval v1; p_cpsexp e2; p_cpsexp e3 ]
  | Let0 (_, x, v, e) -> 
      vert [ horz [ text "let"; text x; text "="; p_cpsval v; text "in" ]; 
             p_cpsexp e ]
  | Let1 (_, x, op, v, e) -> 
      vert [ horz [ text "let"; text x;  text "="; 
                    parens [ text (JavaScript_pretty.render_prefixOp op);
                             p_cpsval v ];
                  text "in" ];
             p_cpsexp e ]
  | Let2 (_, x, op, v1, v2, e) -> 
      vert [ horz [ text "let"; text x; 
                    parens [ text (JavaScript_pretty.render_infixOp op);
                             p_cpsval v1;
                             p_cpsval v2 ];
                    text "in" ];
             p_cpsexp e ]
  | Assign (_, x, v, e) -> 
      vert [ horz [ text x; text ":="; p_cpsval v ];
             p_cpsexp e ]
  | SetProp (_, v1, v2, v3, e) -> 
      vert [ horz [ p_cpsval v1; text "."; p_cpsval v2; text "<-"; 
                    p_cpsval v3 ];
             p_cpsexp e ]
  | Array (_, x, vs, e) ->
      vert [ horz [ text "let"; text x; text "="; 
                    parens (text "array" :: (map p_cpsval vs));
                    text "in" ];
             p_cpsexp e ]
  | Object (_, x, ps, e) ->
      vert [ horz [ text "let"; text x; text "=";
                    parens [ text x; 
                             parens (text "object" :: (map p_prop ps)) ];
                    text "in" ];
             p_cpsexp e ]
  | GetField (_, x, v1, v2, e) ->
      vert [ horz [ text "let"; text x; text "="; 
                    parens [ text "get-field"; p_cpsval v1; p_cpsval v2 ];
                    text "in" ];
             p_cpsexp e ]

and p_prop (x, v) : printer =
  brackets [ text x; p_cpsval v ]

and p_bind (f, args, typ, body) : printer =
  vert  [ horz [ text f; text "=" ];
          parens [ text "lambda"; parens (map text args); p_cpsexp body ] ]

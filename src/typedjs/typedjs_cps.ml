open Prelude
open Typedjs_syntax 

type cpsval =
    Const of Exprjs_syntax.const
  | Id of pos * id

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

let p = (Lexing.dummy_pos, Lexing.dummy_pos)

let mk_id x = Id (p, x)


let rec cps_exp  (exp : exp) (throw : id) (k : cont) : cpsexp = match exp with
    EConst (_, c) -> k (Const c)
  | EId (p, x) -> k (Id (p, x))
  | EArray (_, es) -> 
      cps_exp_list es throw
        (fun vs ->
           let x = new_name () in
             Array (new_node (), x, vs, k (mk_id x)))
  | EObject (_, ps) ->
      cps_exp_list (map thd3 ps) throw
        (fun vs -> 
           let x = new_name () in
             Object (new_node (), x,
                     List.combine (map (fun (f, _, _) -> f) ps) vs,
                     k (mk_id x)))
  | EBracket (_, e1, e2) ->
      cps_exp e1 throw
        (fun v1 ->
           cps_exp e2 throw
             (fun v2 ->
                let x = new_name () in
                  GetField (new_node (), x, v1, v2, k (mk_id x))))
  | EPrefixOp (_, op, e) ->
      cps_exp e throw
        (fun v -> 
           let x = new_name () in
             Let1 (new_node (), x, op, v, k (mk_id x)))
  | EInfixOp (_, op, e1, e2) -> 
      cps_exp e1 throw
        (fun v1 ->
           cps_exp e2 throw
             (fun v2 ->
                let x = new_name () in
                  Let2 (new_node (), x, op, v1, v2, k (mk_id x))))
  | EIf (_, e1, e2, e3) -> 
      cps_exp e1 throw
        (fun v1 ->
           let k' = mk_name "if-cont"
           and r = new_name () in
             Fix (new_node (), 
                  [k', [r], TArrow (TTop, [TTop], TTop), k (mk_id r)],
                  If (new_node (),
                      v1, 
                      cps_exp e2 throw 
                        (fun v -> App (new_node (), mk_id k', [v])),
                      cps_exp e3 throw
                        (fun v -> App (new_node (), mk_id k', [v])))))
  | EApp (_, func, args) -> 
      let k' = mk_name "app-cont"
      and r = new_name () in
        Fix (new_node (),
             [(k', [r], TArrow (TTop, [TTop], TTop), k (mk_id r))],
             cps_exp func throw
               (fun f ->
                  cps_exp_list args throw
                    (fun vs ->
                       App (new_node (), f, (mk_id k') :: (mk_id throw) :: vs))))
  | EFunc (_, args, typ, body) -> 
      let f = mk_name "anon-func"
      and k' = new_name () 
      and throw' = new_name () in
        Fix (new_node (),
             [(f, k' :: throw' :: args, typ, cps_tailexp body throw' k')],
             k (mk_id f))
  | ELet (_, x, e1, e2) ->
      cps_exp e1 throw
        (fun v1 ->
           Let0 (new_node (), x, v1, cps_exp e2 throw k))
  | ERec (binds, body) ->
      Fix (new_node (), map cps_bind binds, cps_exp body throw k)
  | ESeq (_, e1, e2) ->
      cps_exp e1 throw
        (fun _ -> cps_exp e2 throw k)
  | ELabel (_, l, _, e) ->
      let r = new_name () in
        Fix (new_node (),
             [(l, [r], TArrow (TTop, [TTop], TTop), k (mk_id r))],
             cps_tailexp e throw l)
  | EBreak (_, l, e) -> (* drops its own continuation *)
      cps_tailexp e throw l
  | EThrow (_, e) ->
      cps_tailexp e throw throw

(*
  | ETryCatch of pos * exp * id * exp
  | ETryFinally of pos * exp * exp
  | ETypecast of pos * runtime_typs * exp *)

and cps_tailexp (exp : exp) (throw : id) (k : id) : cpsexp = match exp with
    EConst (_, c) -> App (new_node (), mk_id k, [ Const c ])
  | EId (p, x) -> App (new_node (), mk_id k, [ Id (p, x) ])
  | EArray (_, es) -> 
      cps_exp_list es throw
        (fun vs ->
           let x = new_name () in
             Array (new_node (), x, vs, App (new_node (), mk_id k, [ mk_id x ])))
  | EObject (_, ps) ->
      cps_exp_list (map thd3 ps) throw
        (fun vs -> 
           let x = new_name () in
             Object (new_node (), x,
                     List.combine
                       (map (fun (f, _, _) -> f) ps) vs,
                     App (new_node (), mk_id k, [ mk_id x ])))
  | EBracket (_, e1, e2) ->
      cps_exp e1 throw
        (fun v1 ->
           cps_exp e2 throw
             (fun v2 ->
                let x = new_name () in
                  GetField (new_node (), x, v1, v2,
                            App (new_node (), mk_id k, [ mk_id x ]))))
  | EPrefixOp (_, op, e) ->
      cps_exp e throw
        (fun v -> 
           let x = new_name () in
             Let1 (new_node (), x, op, v, 
                   App (new_node (), mk_id k, [ mk_id x ])))
  | EInfixOp (_, op, e1, e2) -> 
      cps_exp e1 throw
        (fun v1 ->
           cps_exp e2 throw
             (fun v2 ->
                let x = new_name () in
                  Let2 (new_node (), x, op, v1, v2, 
                        App (new_node (), mk_id k, [ mk_id x ]))))
  | EIf (_, e1, e2, e3) -> 
      cps_exp e1 throw
        (fun v1 ->
           If (new_node (),
               v1, 
               cps_tailexp e2 throw k, 
               cps_tailexp e3 throw k))
  | EApp (_, func, args) -> 
      cps_exp func throw
        (fun f ->
           cps_exp_list args throw
             (fun vs -> App (new_node (), f, mk_id k :: mk_id throw :: vs)))
  | EFunc (_, args, typ, body) -> 
      let f = new_name ()
      and k' = new_name ()
      and throw' = new_name () in
        Fix (new_node (),
             [(f, k' :: throw' :: args, typ, cps_tailexp body throw' k')],
             App (new_node (), mk_id k, [ mk_id f ]))
  | ELet (_, x, e1, e2) ->
      cps_exp e1 throw
        (fun v1 ->
           Let0 (new_node (), x, v1, cps_tailexp e2 throw k))
  | ERec (binds, body) ->
      Fix (new_node (), map cps_bind binds, cps_tailexp body throw k)
  | ESeq (_, e1, e2) ->
      cps_exp e1 throw
        (fun _ -> 
           cps_tailexp e2 throw k)
  | ELabel (_, l, _, e) ->
      let r = new_name () in
        Fix (new_node (),
             [(l, [r], TArrow (TTop, [TTop], TTop), 
               App (new_node (), mk_id k, [mk_id r]))],
             cps_tailexp e throw k)
  | EBreak (_, l, e) -> (* drops its own continuation *)
      cps_tailexp e throw l
  | EThrow (_, e) ->
      cps_tailexp e throw throw



and cps_bind ((name, typ, e) : id * typ * exp) = match e with
    EFunc (_, args, _, body) ->
      let k = new_name () 
      and throw = new_name () in
        (name, k :: throw :: args, typ, cps_tailexp body throw k)
  | _ -> failwith "cps_bind : expected a function"
  

and cps_exp_list exps throw (k : cpsval list -> cpsexp) = match exps with
    [] -> k []
  | e :: rest ->
      cps_exp e throw
        (fun v ->
           cps_exp_list rest throw
             (fun vs ->
                k (v :: vs)))

let cps (exp : exp) : cpsexp = cps_tailexp exp "%uncaught-exception" "%end"

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
  | Id (_, x) -> IdSet.singleton x

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
  | Id (_, x) -> text x
    
and p_prop (x, v) : printer = brackets [ text x; p_cpsval v ]

let numstr i s : printer = text (string_of_int i ^ ":" ^ s)

let rec p_cpsexp (cpsexp : cpsexp) : printer = match cpsexp with
    Fix (i, binds, body) ->
      parens [ 
        vert [ numstr i "fix"; nest (vert (map p_bind binds)); 
               p_cpsexp body ] ]

  | App (i, f, args ) ->
      parens ( numstr i "app" :: p_cpsval f :: (map p_cpsval args) )
  | If (i, v1, e2, e3) -> 
      parens [ numstr i"if"; p_cpsval v1; p_cpsexp e2; p_cpsexp e3 ]
  | Let0 (i, x, v, e) -> 
      vert [ horz [ numstr i"let"; text x; text "="; p_cpsval v; text "in" ]; 
             p_cpsexp e ]
  | Let1 (i, x, op, v, e) -> 
      vert [ horz [ numstr i"let"; text x;  text "="; 
                    parens [ text (JavaScript_pretty.render_prefixOp op);
                             p_cpsval v ];
                  text "in" ];
             p_cpsexp e ]
  | Let2 (i, x, op, v1, v2, e) -> 
      vert [ horz [ numstr i"let"; text x; 
                    parens [ text (JavaScript_pretty.render_infixOp op);
                             p_cpsval v1;
                             p_cpsval v2 ];
                    text "in" ];
             p_cpsexp e ]
  | Assign (i, x, v, e) -> 
      vert [ horz [ numstr i ("asgn:"^x); text ":="; p_cpsval v ];
             p_cpsexp e ]
  | SetProp (i, v1, v2, v3, e) -> 
      vert [ horz [ numstr i "prop"; p_cpsval v1; text "."; 
                    p_cpsval v2; text "<-"; 
                    p_cpsval v3 ];
             p_cpsexp e ]
  | Array (i, x, vs, e) ->
      vert [ horz [ numstr i"let"; text x; text "="; 
                    parens (text "array" :: (map p_cpsval vs));
                    text "in" ];
             p_cpsexp e ]
  | Object (i, x, ps, e) ->
      vert [ horz [ numstr i"let"; text x; text "=";
                    parens [ text x; 
                             parens (text "object" :: (map p_prop ps)) ];
                    text "in" ];
             p_cpsexp e ]
  | GetField (i, x, v1, v2, e) ->
      vert [ horz [ numstr i"let"; text x; text "="; 
                    parens [ text "get-field"; p_cpsval v1; p_cpsval v2 ];
                    text "in" ];
             p_cpsexp e ]

and p_prop (x, v) : printer =
  brackets [ text x; p_cpsval v ]

and p_bind (f, args, typ, body) : printer =
  parens [ vert 
             [ text f;
               parens [ text "lambda"; parens (map text args);
                        (fun fmt -> Typedjs_pretty.pretty_typ fmt typ);
                        p_cpsexp body ] ]
         ]

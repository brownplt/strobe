open Prelude
open Lambdajs_syntax

type cpsval =
    Const of Exprjs_syntax.const
  | Array of cpsval list
  | Object of (string * cpsval) list
  | Id of id

type node = int * pos


type cpsexp =
    Fix of node * lambda list * cpsexp
  | App of node * cpsval * cpsval list
  | If of node * cpsval * cpsexp * cpsexp
  | Let0 of node * id * cpsval * cpsexp (* load immediate / reg-reg move *)
  | Let1 of node * id * op1 * cpsval * cpsexp
  | Let2 of node * id * op2 * cpsval * cpsval * cpsexp
  | GetField of node * id * cpsval * cpsval * cpsexp
  | DeleteField of node * id * cpsval * cpsval * cpsexp
  | UpdateField of node * id * cpsval * cpsval * cpsval * cpsexp
  | Ref of node * id * cpsval * cpsexp
  | SetRef of node * cpsval * cpsval * cpsexp
  | Deref of node * id * cpsval * cpsexp

and lambda = id * id list * cpsexp

(*****************************************************************************)

module Cps = struct

  type cont = cpsval -> cpsexp

  let next_node_int = ref 0

  let mk_node (p : pos) : node = 
    let n = !next_node_int in
      incr next_node_int;
      n, p

  let next_name_int = ref 0

  let mk_name () : id = 
    let n = !next_name_int in
      incr next_name_int;
      "%cps" ^ string_of_int n
  
  let rec cps (exp : exp) (throw : id) (k : cont) : cpsexp = match exp with
      EConst (p, c) -> k (Const c)
    | EId (p, x) -> k (Id x)
    | EArray (p, es) -> cps_list es throw (fun vs -> k (Array vs))
    | EObject (_, ps) ->
        cps_list (map thd3 ps) throw 
          (fun vs -> k (Object (List.combine (map snd3 ps) vs)))
    | ESeq (p, e1, e2) -> cps e1 throw (fun _ -> cps e2 throw k)
    | EGetField (p, e1, e2) ->
        cps e1 throw
          (fun v1 ->
             cps e2 throw
               (fun v2 ->
                  let x = mk_name () in
                    GetField (mk_node p, x, v1, v2, k (Id x))))
    | EDeleteField (p, e1, e2) ->
        cps e1 throw
          (fun v1 ->
             cps e2 throw
               (fun v2 ->
                  let x = mk_name () in
                    DeleteField (mk_node p, x, v1, v2, k (Id x))))
    | EUpdateField (p, e1, e2, e3) ->
        cps e1 throw
          (fun v1 ->
             cps e2 throw
               (fun v2 ->
                  cps e3 throw
                    (fun v3 ->
                       let x = mk_name () in
                         UpdateField (mk_node p, x, v1, v2, v3, k (Id x)))))
    | EOp1 (p, op, e) ->
        cps e throw
          (fun v ->
             let r = mk_name () in
               Let1 (mk_node p, r, op, v, k (Id r)))
    | EOp2 (p, op, e1, e2) ->
        cps e1 throw
          (fun v1 ->
             cps e2 throw
               (fun v2 ->
                  let r = mk_name () in
                    Let2 (mk_node p, r, op, v1, v2, k (Id r))))
    | ELet (p, x, e1, e2) ->
        cps e1 throw
          (fun v1 ->
             Let0 (mk_node p, x, v1,
                   cps e2 throw k))
    | EIf (p, e1, e2, e3) ->
        let cont = mk_name () in
          Fix (mk_node p, 
               [ let r = mk_name () in
                   (cont, [r], k (Id r) ) ],
               cps e1 throw
                 (fun v1 ->
                    If (mk_node p, v1, 
                        tailcps e2 throw cont,
                        tailcps e3 throw cont)))
    | ERef (p, e) ->
        cps e throw
          (fun v -> 
             let r = mk_name () in
               Ref (mk_node p, r, v, k (Id r)))
    | EDeref (p, e) ->
        cps e throw
          (fun v -> 
             let r = mk_name () in
               Deref (mk_node p, r, v, k (Id r)))
    | ESetRef (p, e1, e2) ->
        cps e1 throw
          (fun v1 ->
             cps e2 throw
               (fun v2 ->
                  SetRef (mk_node p, v1, v2, k v1)))
    | EFix (p, binds, body) ->
        Fix (mk_node p,
             map cps_bind binds,
             cps body throw k)
  | ELambda (p, args, body) -> 
      let f = mk_name ()
      and cont = mk_name ()
      and throw = mk_name () in
        Fix (mk_node p,
             [ (f, cont :: throw :: args, tailcps body throw cont) ],
             k (Id f))
  | EApp (p, f, args) ->
      let cont = mk_name () in
        Fix (mk_node p, 
             [ let r = mk_name () in (cont, [r], k (Id r)) ],
             cps f throw
               (fun fv ->
                  cps_list args throw
                    (fun argsv ->
                       App (mk_node p, fv, Id cont :: Id throw :: argsv))))
  | ETryCatch (p, body, ELambda (p', [exn], catch_body)) -> 
      let cont = mk_name ()  in
        Fix (mk_node p,
             [ let r = mk_name () in (cont, [r], k (Id r)) ],
             let throw' = mk_name () in
               Fix (mk_node p, 
                    [ throw', [exn], tailcps catch_body throw cont ],
                    tailcps body throw' cont))
  | ETryCatch _ -> failwith "cps : ill-formed catch block"
  | EThrow (p, e) -> 
      tailcps e throw throw (* that's right *)
  | ETryFinally (p, body, finally) ->
      let cont = mk_name () in
        Fix (mk_node p,
             [ let r = mk_name () in (cont, [r], k (Id r)) ],
             let normal_exit = mk_name () 
             and throw_exit = mk_name () in
               (* TODO: account for breaks *)
               Fix (mk_node p,
                    [ (normal_exit, [mk_name ()], tailcps finally throw cont);
                      let exn = mk_name () in
                        (throw_exit, [exn],
                         cps finally throw 
                           (fun _ -> App (mk_node p, Id throw, [Id exn]))) ],
                    tailcps body throw_exit normal_exit))
  | ELabel (p, lbl, e) ->
      Fix (mk_node p,
           [ let r = mk_name () in
               (lbl, [r], k (Id r)) ],
           tailcps e throw lbl)
  | EBreak (p, lbl, e) ->
      cps e throw (fun v -> App (mk_node p, Id lbl, [v]))

  and cps_list (exps : exp list) (throw : id) (k : cpsval list -> cpsexp) =
    match exps with
        [] -> k []
      | e :: exps' ->
          cps e throw
            (fun v ->
               cps_list exps' throw
                 (fun vs ->
                    k (v :: vs)))

  and tailcps (exp : exp) (throw : id) (return : id) : cpsexp = 
    cps exp throw 
      (fun v -> App (mk_node (Lexing.dummy_pos, Lexing.dummy_pos),
                     Id return, [ v ]))

  and cps_bind ((f, exp) : id * exp) = match exp with
      ELambda (p, args, body) ->
        let cont = mk_name () in
        let throw = mk_name () in
          (f, cont :: throw :: args, tailcps body throw cont)
    | _ -> failwith "cps: ill-formed fix-binding"

end (* struct Cps *)


module Pretty = struct

  open Format
  open FormatExt

  let rec p_cpsval (cpsval : cpsval) : printer = match cpsval with
      Const c -> Exprjs_pretty.p_const c
    | Array vs -> parens (text "array" :: (map p_cpsval vs))
    | Object ps -> parens (text "object" :: (map p_prop ps))
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
                      parens [ p_op1 op; p_cpsval v ];
                      text "in" ];
               p_cpsexp e ]
    | Let2 (_, x, op, v1, v2, e) -> 
        vert [ horz [ text "let"; text x; 
                      parens [ p_op2 op; p_cpsval v1; p_cpsval v2 ];
                      text "in" ];
               p_cpsexp e ]
    | GetField (_, x, v1, v2, e) ->
        vert [ horz [ text "let"; text x; text "="; 
                      parens [ text "get-field"; p_cpsval v1; p_cpsval v2 ];
                      text "in" ];
               p_cpsexp e ]
    | DeleteField (_, x, v1, v2, e) ->
        vert [ horz [ text "let"; text x; text "="; 
                      parens [ text "delete-field"; p_cpsval v1; p_cpsval v2 ];
                      text "in" ];
               p_cpsexp e ]
    | UpdateField (_, x, v1, v2, v3, e) ->
        vert [ horz [ text "let"; text x; text "="; 
                      parens [ text "update-field"; p_cpsval v1; p_cpsval v2;
                               p_cpsval v3 ];
                      text "in" ];
               p_cpsexp e ]
    | Ref (_, x, v, e) ->
        vert [ horz [ text "let"; text x; text "="; 
                      parens [ text "ref"; p_cpsval v ];
                      text "in" ];
               p_cpsexp e ]
    | Deref (_, x, v, e) ->
        vert [ horz [ text "let"; text x; text "="; 
                      parens [ text "deref"; p_cpsval v ];
                      text "in" ];
               p_cpsexp e ]
    | SetRef (_, v1, v2, e) ->
        vert [ horz [ parens [ text "set-ref"; p_cpsval v1; p_cpsval v2 ];
                      text ";" ];
               p_cpsexp e ]
          
  and p_prop (x, v) : printer =
    brackets [ p_cpsval x; p_cpsval v ]
      
  and p_bind (f, args, body) : printer =
    vert  [ horz [ text f; text "=" ];
            parens [ text "lambda"; parens (map text args); p_cpsexp body ] ]

end

let cpsexp_idx (cpsexp : cpsexp) = match cpsexp with
  | Fix ((n, _), _, _) -> n
  | App ((n, _), _, _) -> n
  | If ((n, _), _, _, _) -> n
  | Let0 ((n, _), _, _, _) -> n
  | Let1 ((n, _), _, _, _, _) -> n
  | Let2 ((n, _), _, _, _, _, _) -> n
  | GetField ((n, _), _, _, _, _) -> n
  | DeleteField ((n, _), _, _, _, _) -> n
  | UpdateField ((n, _), _, _, _, _, _) -> n
  | Ref ((n, _), _, _, _) -> n
  | SetRef ((n, _), _, _, _) -> n
  | Deref ((n, _), _, _, _) -> n

let lambda_name (f, _, _) = f

let p_cpsexp  = Pretty.p_cpsexp      

let cps (exp : exp) : cpsexp = 
  Cps.tailcps exp "%uncaught-exception" "%return-value"

let mk_node = Cps.mk_node


let rec fv (cpsexp : cpsexp) : IdSet.t = match cpsexp with
  |  Fix (_, binds, body) ->
       let bound_ids = IdSetExt.from_list (map fst3 binds) in
         IdSet.diff (IdSetExt.unions (fv body :: map fv_bind binds))
         bound_ids
  | App (_, v, vs) -> IdSetExt.unions (map fv_val (v :: vs))
  | If (_, v1, e2, e3) ->
      IdSetExt.unions [ fv_val v1; fv e2; fv e3 ]
  | Let0 (_, x, v, e) -> 
      IdSet.union (fv_val v)
        (IdSet.remove x (fv e))
  | Let1 (_, x, _, v, e) -> 
      IdSet.union (fv_val v)
        (IdSet.remove x (fv e))
  | Let2 (_, x, _, v1, v2, e) ->
      IdSetExt.unions [ fv_val v1; fv_val v2; IdSet.remove x (fv e) ]
  | GetField (_, x, v1, v2, e) -> 
      IdSetExt.unions [ fv_val v1; fv_val v2; IdSet.remove x (fv e) ]
  | DeleteField (_, x, v1, v2, e) -> 
      IdSetExt.unions [ fv_val v1; fv_val v2; IdSet.remove x (fv e) ]
  | UpdateField(_, x, v1, v2, v3, e) -> 
      IdSetExt.unions [ fv_val v1; fv_val v2; fv_val v3; IdSet.remove x (fv e) ]
  | Ref (_, x, v, e) -> 
      IdSetExt.unions [ fv_val v; IdSet.remove x (fv e) ]
  | SetRef (_, v1, v2, e) ->
      IdSetExt.unions [ fv_val v1; fv_val v2; fv e ]
  | Deref (_, x, v, e) -> 
      IdSetExt.unions [ fv_val v; IdSet.remove x (fv e) ]

and fv_val (cpsval : cpsval) = match cpsval with
  | Const _ -> IdSet.empty
  | Array vs -> IdSetExt.unions (map fv_val vs)
  | Object ps -> IdSetExt.unions (map (fun (_, v) -> fv_val v) ps)
  | Id x -> IdSet.singleton x

and fv_bind (_, args, body) =
  IdSet.diff (fv body) (IdSetExt.from_list args)

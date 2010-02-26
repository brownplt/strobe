open Prelude
open Lambdajs_syntax

type cpsval =
    Const of Exprjs_syntax.const
  | Array of cpsval list
  | Object of (string * cpsval) list
  | Id of id

type node = int * pos

type bindexp =
  | Let of cpsval
  | Op1 of op1 * cpsval
  | Op2 of op2 * cpsval * cpsval
  | UpdateField of cpsval * cpsval * cpsval

type cpsexp =
  | Fix of node * lambda list * cpsexp
  | App of node * cpsval * cpsval list
  | If of node * cpsval * cpsexp * cpsexp
  | Bind of node * id * bindexp * cpsexp

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
    | EUpdateField (p, e1, e2, e3) ->
        cps e1 throw
          (fun v1 ->
             cps e2 throw
               (fun v2 ->
                  cps e3 throw
                    (fun v3 ->
                       let x = mk_name () in
                        Bind (mk_node p, x, UpdateField (v1, v2, v3),
                              k (Id x)))))
    | EOp1 (p, op, e) ->
        cps e throw
          (fun v ->
             let r = mk_name () in
               Bind (mk_node p, r, Op1 (op, v), k (Id r)))
    | EOp2 (p, op, e1, e2) ->
        cps e1 throw
          (fun v1 ->
             cps e2 throw
               (fun v2 ->
                  let r = mk_name () in
                    Bind (mk_node p, r, Op2 (op, v1, v2), k (Id r))))
    | ELet (p, x, e1, e2) ->
        cps e1 throw
          (fun v1 ->
             Bind (mk_node p, x, Let v1,
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

let cpsexp_idx (cpsexp : cpsexp) = match cpsexp with
  | Fix ((n, _), _, _) -> n
  | App ((n, _), _, _) -> n
  | If ((n, _), _, _, _) -> n
  | Bind ((n, _), _, _, _) -> n

module Pretty = struct

  open Format
  open FormatExt

  module H = Hashtbl

  let rec p_cpsval (cpsval : cpsval) : printer = match cpsval with
      Const c -> Exprjs_pretty.p_const c
    | Array vs -> parens (text "array" :: (map p_cpsval vs))
    | Object ps -> parens (text "object" :: (map p_prop ps))
    | Id x -> text x
    
  and p_prop (x, v) : printer = brackets [ text x; p_cpsval v ]

  let p_bindexp (bindexp : bindexp) : printer = match bindexp with
    | Let v -> p_cpsval v
    | Op1 (op, v1) -> parens [ p_op1 op; p_cpsval v1 ]
    | Op2 (op, v1, v2) -> parens [ p_op2 op; p_cpsval v1; p_cpsval v2 ]
    | UpdateField (v1, v2, v3) -> 
        parens [ p_cpsval v1; p_cpsval v2; p_cpsval v3 ]

  let rec p_cpsexp (cpsexp : cpsexp) : printer = match cpsexp with
      Fix (_, binds, body) ->
        vert [ text "fix"; nest (vert (map p_bind binds)); p_cpsexp body ]
    | App (_, f, args ) ->
        parens ( text "app" :: p_cpsval f :: (map p_cpsval args) )
    | If (_, v1, e2, e3) -> 
        parens [ text "if"; p_cpsval v1; p_cpsexp e2; p_cpsexp e3 ]
    | Bind (_, x, b, k) ->
        vert [ horz [ text "let"; text x; text "="; p_bindexp b; text "in" ]; 
               p_cpsexp k ]
      
  and p_bind (f, args, body) : printer =
    vert  [ horz [ text f; text "=" ];
            parens [ text "lambda"; parens (map text args); p_cpsexp body ] ]


  let svg_cpsexp (e : cpsexp) (fmt : formatter) : (int, int * int) H.t =
    let pos_tbl = H.create 100 in

    let rec  cpsexp (e : cpsexp) : printer = 
      printf "position of %d is %d, %d\n" (cpsexp_idx e) !svg_col !svg_line;
      H.add pos_tbl (cpsexp_idx e) (!svg_col, !svg_line);
      match e with
        |  Fix (_, binds, body) ->
             vert [ text "fix"; nest (vert (map bind binds)); cpsexp body ]
        | App (_, f, args ) ->
            parens ( text "app" :: p_cpsval f :: (map p_cpsval args) )
        | If (_, v1, e2, e3) -> 
            parens [ text "if"; p_cpsval v1; p_cpsexp e2; cpsexp e3 ]
        | Bind (_, x, b, k) ->
            vert [ horz [ text "let"; text x; text "="; p_bindexp b; 
                          text "in" ]; 
                   cpsexp k ]
      
    and bind (f, args, body) : printer =
      vert  [ horz [ text f; text "=" ];
              parens [ text "lambda"; parens (map text args); cpsexp body ] ]
    in cpsexp e fmt;
      pos_tbl


end


let lambda_name (f, _, _) = f

let svg_cpsexp = Pretty.svg_cpsexp

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
  | Bind (_, x, b, e) -> 
      IdSet.union (fv_bindexp b)
        (IdSet.remove x (fv e))

and fv_val (cpsval : cpsval) = match cpsval with
  | Const _ -> IdSet.empty
  | Array vs -> IdSetExt.unions (map fv_val vs)
  | Object ps -> IdSetExt.unions (map (fun (_, v) -> fv_val v) ps)
  | Id x -> IdSet.singleton x

and fv_bindexp (bindexp : bindexp) = match bindexp with
  | Let v -> fv_val v
  | Op1 (_, v) -> fv_val v
  | Op2 (_, v1, v2) -> IdSet.union (fv_val v1) (fv_val v2)
  | UpdateField (v1, v2, v3) -> 
      IdSet.union (fv_val v1) (IdSet.union (fv_val v2) (fv_val v3))

and fv_bind (_, args, body) =
  IdSet.diff (fv body) (IdSetExt.from_list args)

let fv_immediate (cpsexp : cpsexp) : IdSet.t = match cpsexp with
  |  Fix (_, binds, body) -> IdSet.empty
  | App (_, v, vs) -> IdSetExt.unions (map fv_val (v :: vs))
  | If (_, v1, e2, e3) -> fv_val v1
  | Bind (_, x, b, e) -> fv_bindexp b

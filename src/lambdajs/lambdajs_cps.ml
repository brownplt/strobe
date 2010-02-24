open Prelude
open Lambdajs_syntax

type cpsval =
    Const of Exprjs_syntax.const
  | Array of cpsval list
  | Object of (string * cpsval) list
  | Id of id

type node = int * pos

type cpsexp =
    Fix of node * (id * id list * cpsexp) list * cpsexp
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

let cps (exp : exp) : cpsexp = 
  Cps.tailcps exp "%uncaught-exception" "%return-value"

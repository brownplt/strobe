open Prelude
open Typedjs_syntax 

type cpsval =
  | Const of JavaScript_syntax.const
  | Id of pos * id

type node = int

type op1 = 
  | Op1Prefix of id
  | Deref
  | Ref

type op2 =
  | Op2Infix of id
  | GetField
  | DeleteField
  | SetRef

type bindexp =
  | Let of cpsval
  | Op1 of op1 * cpsval
  | Op2 of op2 * cpsval * cpsval
  | Object of (string * cpsval) list
  | Array of cpsval list
  | UpdateField of cpsval * cpsval * cpsval

type cpsexp =
    Fix of node * (id * id list * typ * cpsexp) list * cpsexp
  | App of node * cpsval * cpsval list
  | If of node * cpsval * cpsexp * cpsexp
  | Bind of node * id * bindexp * cpsexp

(******************************************************************************)

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

let p = (Lexing.dummy_pos, Lexing.dummy_pos)

let mk_id x = Id (p, x)

(* [ext_typ] extends the arrow type on a function to account for additional
   arguments added by the CPS transformation: the continuation and exception
   handler. *)
let ext_typ typ = match typ with
  | TArrow (this_typ, arg_typs, result_typ) ->
      (* Note that all return types are TBot. (i.e. does not return) *)
      let cont_typ = TArrow (TTop, [result_typ], TBot)
        (* We can throw anything. *)
      and throw_typ = TArrow (TTop, [TTop], TBot) 
      and this_typ = TObject [] in
      TArrow (this_typ, cont_typ :: throw_typ :: this_typ :: arg_typs, TBot)
  | _ -> failwith "ext_typ expected an arrow type"


type cont =
  | Cont of (cpsval -> cpsexp)
  | Jmp of id

let ret cont v = match cont with
  | Cont k -> k v
  | Jmp l -> App (new_node (), mk_id l, [ v ])

let bind_cont cont fn = match cont with
  | Jmp l -> fn l
  | Cont k ->
      let k' = new_name () in
      let r = new_name () in
        Fix (new_node (), 
             [k', [r], TArrow (TTop, [TTop], TTop), k (mk_id r)],
             fn k')      

let rec cps_exp  (exp : exp) (throw : id) (k : cont) : cpsexp = match exp with
  | EConst (_, c) -> ret k (Const c)
  | EId (p, x) -> ret k (Id (p, x))
  | EArray (_, es) -> 
      cps_exp_list es throw
        (fun vs ->
           let x = new_name () in
             Bind (new_node (), x, Array vs, ret k (mk_id x)))
  | EEmptyArray _ ->
      let x = new_name () in
        Bind (new_node (), x, Array [], ret k (mk_id x))
  | EObject (_, ps) ->
      cps_exp_list (map snd2 ps) throw
        (fun vs -> 
           let x = new_name () in
             Bind (new_node (), x, 
                   Object (List.combine (map fst2 ps) vs),
                   ret k (mk_id x)))
  | EBracket (_, e1, e2) ->
      cps' e1 throw
        (fun v1 ->
           cps' e2 throw
             (fun v2 ->
                let x = new_name () in
                  Bind (new_node (), x, Op2 (GetField, v1, v2),
                        ret k (mk_id x))))
  | EPrefixOp (_, op, e) ->
      cps' e throw
        (fun v -> 
           let x = new_name () in
             Bind (new_node (), x, Op1 (Op1Prefix op, v),
                   ret k (mk_id x)))
  | EInfixOp (_, op, e1, e2) -> 
      cps' e1 throw
        (fun v1 ->
           cps' e2 throw
             (fun v2 ->
                let x = new_name () in
                  Bind (new_node (), x, Op2 (Op2Infix op, v1, v2),
                        ret k (mk_id x))))
  | EIf (_, e1, e2, e3) -> 
      cps' e1 throw
        (fun v1 ->
           bind_cont k
             (fun k' ->
                If (new_node (),
                    v1, 
                    cps_exp e2 throw (Jmp k'),
                    cps_exp e3 throw (Jmp k'))))
  | EApp (_, EBracket (_, obj, prop), args) ->
      bind_cont k
        (fun k' ->
           cps' obj throw
             (fun objv ->
                cps' prop throw
                  (fun propv ->
                     cps_exp_list args throw
                       (fun argvs ->
                          let meth = mk_name "%method" in
                            Bind (new_node (), meth, 
                                  Op2 (GetField, objv, propv),
                                  App (new_node (), mk_id meth,
                                       mk_id k' :: mk_id throw :: objv 
                                       :: argvs))))))
  | EApp (_, func, args) -> 
      bind_cont k
        (fun k' ->
           cps' func throw
             (fun f ->
                cps_exp_list args throw
                  (fun vs ->
                     App (new_node (), f,
                          (mk_id k') :: (mk_id throw) :: 
                            (mk_id "%global") :: vs))))
  | EFunc (_, args, typ, body) -> 
      let f = mk_name "anon-func"
      and k' = new_name () 
      and throw' = new_name () in
        Fix (new_node (),
             [(f, k' :: throw' :: "%this" :: args, ext_typ typ, 
               cps_exp body throw' (Jmp k'))],
             ret k (mk_id f))
  | ELet (_, x, e1, e2) ->
      cps' e1 throw
        (fun v1 ->
           Bind (new_node (), x, Let v1, cps_exp e2 throw k))
  | ERef (_, _, e) ->
      cps' e throw
        (fun v ->
           let x = new_name () in
             Bind (new_node (), x, Op1 (Ref, v), ret k (mk_id x)))
  | EDeref (_, e) ->
      cps' e throw
        (fun v ->
           let x = new_name () in
             Bind (new_node (), x, Op1 (Deref, v), ret k (mk_id x)))
  | ESetRef (_, e1, e2) ->
      cps' e1 throw
        (fun v1 ->
           cps' e2 throw
             (fun v2 ->
                let x = new_name () in
                  Bind (new_node (), x, Op2 (SetRef, v1, v2),
                        ret k v2)))
  | ERec (binds, body) ->
      Fix (new_node (), map cps_bind binds, cps_exp body throw k)
  | ESeq (_, e1, e2) ->
      cps' e1 throw (fun _ -> cps_exp e2 throw k)
  | ELabel (_, l, _, e) ->
      let r = new_name () in
        Fix (new_node (),
             [(l, [r], TArrow (TTop, [TTop], TTop), ret k (mk_id r))],
             cps_exp e throw (Jmp l))
  | EBreak (_, l, e) -> (* drops its own continuation *)
      cps_exp e throw (Jmp l)
  | EThrow (_, e) ->
      cps_exp e throw (Jmp throw)
  | EThis _ -> ret k (Id (p, "%this"))
  | ENew (_, constr, args) ->
      let k' = mk_name "app-cont"
      and obj = new_name () in
        Bind (new_node (), obj, Object [],
              Fix (new_node (),
                   [(k', [new_name ()], TArrow (TTop, [TTop], TTop), 
                     ret k (mk_id obj))],
                   cps_exp_list args throw
                     (fun argvs ->
                        App (new_node (), mk_id constr,
                             mk_id k' :: mk_id throw :: mk_id obj ::
                               argvs))))
  | ESubsumption (_, _, e) -> cps_exp e throw k
  | EDowncast (_, _, e) -> cps_exp e throw k
  | ETypecast (_, _, e) -> cps_exp e throw k
  | ETypAbs (_, _, _, e) -> cps_exp e throw k
  | ETypApp (_, e, _) -> cps_exp e throw k
  | ETryCatch (p, body, exn, catch_body) ->
      let throw' = mk_name "throw-cont" in
        bind_cont k
          (fun cont ->
             Fix (new_node (),
                  [throw', [exn], TArrow (TTop, [TTop], TBot),
                   cps_exp catch_body throw (Jmp cont)],
                  cps_exp body throw' (Jmp cont)))

and cps_bind ((name, typ, e) : id * typ * exp) = match e with
    EFunc (_, args, _, body) ->
      let k = new_name () 
      and throw = new_name () in
        (name, k :: throw :: "%this" :: args,
         ext_typ typ, cps_exp body throw (Jmp k))
  | _ -> failwith "cps_bind : expected a function"

and cps_exp_list exps throw (k : cpsval list -> cpsexp) = match exps with
    [] -> k []
  | e :: rest ->
      cps' e throw
        (fun v ->
           cps_exp_list rest throw
             (fun vs ->
                k (v :: vs)))

and cps' (e : exp) throw (f : cpsval -> cpsexp) = cps_exp e throw (Cont f)


let rec cps (def : def) : cpsexp = match def with
  | DEnd -> App (new_node (), Id (p, "%end"), [])
  | DExp (e, d) -> cps' e "%uncaught-exception" (fun _ -> cps d)
  | DLet (_, x, e, d) ->
      cps' e "%uncaught-exception"
        (fun v ->
           Bind (new_node (), x, Let v, cps d))
  | DRec (binds, d) ->
      Fix (new_node (), map cps_bind binds, cps d)
  (* ignore the inits, since we punt on them anyway *)
  | DConstructor (cexp, d) -> 
      let p = cexp.constr_pos in
        Fix (new_node (), [cps_bind (cexp.constr_name, cexp.constr_typ,
                                     EFunc (p,
                                            cexp.constr_args, 
                                            TTop, 
                                            cexp.constr_exp))], 
             (* add setting the prototype as a plain expression: *)
             cps (DExp (ESetRef (p,
                                 EBracket (p,
                                           EDeref (p, 
                                                   EId (p, cexp.constr_name)),
                                           EConst (p, JavaScript_syntax.CString 
                                                     "prototype")),
                                 cexp.constr_prototype), 
                        d)))
  | DExternalMethod (p, cname, mid, me, d) ->
      (* we would punt on setting to .proto anyway, so just cps the expr
         and move on *)
      cps' me "%uncaught-exception" (fun _ -> cps d)

let node_of_cpsexp (cpsexp : cpsexp) : node = match cpsexp with
    Fix (n, _, _) -> n
  | App (n, _, _) -> n
  | Bind (n, _, _, _) -> n
  | If (n, _, _, _) -> n

(******************************************************************************)

let fv_val (cpsval : cpsval) : IdSet.t = match cpsval with
    Const _ -> IdSet.empty
  | Id (_, x) -> IdSet.singleton x

let fv_prop (_, v) = fv_val v

let fv_bindexp (e : bindexp) : IdSet.t = match e with
  | Let v -> fv_val v
  | Op1 (_, v) -> fv_val v
  | Op2 (_, v1, v2) -> IdSet.union (fv_val v1) (fv_val v2)
  | Object ps -> IdSetExt.unions (map (fun (_, v) -> fv_val v) ps)
  | Array vs -> IdSetExt.unions (map fv_val vs)
  | UpdateField (v1, v2, v3) ->
      IdSet.union (fv_val v1) (IdSet.union (fv_val v2) (fv_val v3))

let rec fv_cpsexp (cpsexp : cpsexp) : IdSet.t = match cpsexp with
    Fix (_, binds, body) ->
      IdSetExt.unions (fv_cpsexp body :: (map fv_bind binds))
  | App (_, f, vs) ->
      IdSetExt.unions (map fv_val (f :: vs))
  | If (_, v, e1, e2) -> 
      IdSet.union (fv_val v) (IdSet.union (fv_cpsexp e1) (fv_cpsexp e2))
  | Bind (_, x, e, k) -> 
      IdSet.union (fv_bindexp e) (IdSet.remove x (fv_cpsexp k))

and fv_bind (_, args, _, e) =
  IdSet.diff (fv_cpsexp e) (IdSetExt.from_list args)

let rec esc_cpsexp (cpsexp : cpsexp) : IdSet.t = match cpsexp with
    Fix (_, binds, body) ->
      IdSetExt.unions (esc_cpsexp body :: (map esc_bind binds))
  | App (_, _, vs) ->
      (* An identifier in function position does not escape. *)
      IdSetExt.unions (map fv_val vs)
  | If (_, _, e1, e2) -> IdSet.union (esc_cpsexp e1) (esc_cpsexp e2)
  | Bind (_, x, e, k) -> 
      IdSet.union (fv_bindexp e) (IdSet.remove x (esc_cpsexp k))

and esc_bind (_, args, _, e) = 
  IdSet.diff (esc_cpsexp e) (IdSetExt.from_list args)



(******************************************************************************)

open Format
open FormatExt

let rec p_cpsval (cpsval : cpsval) : printer = match cpsval with
    Const c -> JavaScript.Pretty.p_const c
  | Id (_, x) -> text x
    
and p_prop (x, v) : printer = brackets (horz [ text x; p_cpsval v ])

let numstr i s : printer = text (string_of_int i ^ ":" ^ s)

let p_op1 op = match op with
  | Op1Prefix o -> text o
  | Deref -> text "deref"
  | Ref -> text "ref"

let p_op2 op = match op with
  | Op2Infix o -> text o
  | GetField -> text "get-field"
  | DeleteField -> text "delete-field"
  | SetRef -> text "set-ref!"

let p_bindexp (exp : bindexp) : printer = match exp with
  | Let v -> p_cpsval v
  | Op1 (op, v) ->  parens (horz [ p_op1 op; p_cpsval v ])
  | Op2 (op, v1, v2) -> parens (horz [ p_op2 op ; p_cpsval v1; p_cpsval v2 ])
  | Object ps -> parens (vert (text "object" ::  (map p_prop ps)))
  | Array vs -> parens (horz (text "array" :: map p_cpsval vs))
  | UpdateField (v1, v2, v3) ->
      parens (horz [ text "update-field"; p_cpsval v1; p_cpsval v2;
                     p_cpsval v3 ])


let rec p_cpsexp (cpsexp : cpsexp) : printer = match cpsexp with
    Fix (i, binds, body) ->
      parens
        (vert [ numstr i "fix"; nest (vert (map p_bind binds)); 
                p_cpsexp body ])

  | App (i, f, args ) ->
      parens (horz (numstr i "app" :: p_cpsval f :: (map p_cpsval args)))
  | If (i, v1, e2, e3) -> 
      parens (vert [ numstr i"if"; p_cpsval v1; p_cpsexp e2; p_cpsexp e3 ])
  | Bind (n, x, e, cont) ->
      parens (vert [ horz [ numstr n "let"; text x; p_bindexp e ];
                     p_cpsexp cont ])

and p_bind (f, args, typ, body) : printer =
  parens ( vert 
             [ text f;
               parens 
                 (vert 
                    [ text "lambda"; parens (horz (map text args));
                      Typedjs_syntax.Pretty.p_typ typ;
                      p_cpsexp body ]) ])

open Prelude
open Typedjs_syntax 
open TypImpl

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
  | AssertTyp of typ * cpsval

and cpsexp =
  | Rec of node * (id * cpsval) list * cpsexp
  | App of node * cpsval * cpsval list
  | If of node * cpsval * cpsexp * cpsexp
  | Bind of node * id * bindexp * cpsexp

and cpsval =
  | Const of JavaScript_syntax.const
  | Id of pos * id
  | Lambda of id list * cpsexp
  | ExternalLambda of typ

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

(* [cont] and [ret] preserve tail calls in the CPS translation. *)
type cont =
  | Cont of (cpsval -> cpsexp)
  | Jmp of id

let ret (cont : cont) (v : cpsval) : cpsexp = match cont with
  | Cont k -> k v
  | Jmp l -> App (new_node (), mk_id l, [ v ])

let bind_cont cont fn = match cont with
  | Jmp l -> fn l
  | Cont k ->
      let k' = new_name () in
      let r = new_name () in
      Bind (new_node (), 
            k', Let (Lambda ([r], k (mk_id r))),
            fn k')

let rec cps_exp  (exp : exp) (throw : id) (k : cont) : cpsexp = match exp with
  | EConst (_, c) -> ret k (Const c)
  | EBot _ -> ret k (Const JavaScript_syntax.CUndefined)
  | EId (p, x) -> ret k (Id (p, x))
  | EArray (_, es) -> 
    cps_exp_list es throw
      (fun vs ->
        let x = new_name () in
        Bind (new_node (), x, Array vs, ret k (mk_id x)))
  | EObject (_, ps) ->
    cps_exp_list (map snd2 ps) throw
      (fun vs -> 
        let x = new_name () in
        Bind (new_node (), 
              x, Object (List.combine (map fst2 ps) vs),
              ret k (mk_id x)))
  | EBracket (_, e1, e2) ->
    cps' e1 throw
      (fun v1 ->
        cps' e2 throw
          (fun v2 ->
            let x = new_name () in
            Bind (new_node (), 
                  x, Op2 (GetField, v1, v2),
                  ret k (mk_id x))))
  | EUpdate (_, e1, e2, e3) ->
    cps' e1 throw
      (fun v1 ->
        cps' e2 throw
          (fun v2 ->
            cps' e3 throw
              (fun v3 ->
                let x = new_name () in
                Bind (new_node (), 
                      x, UpdateField (v1, v2, v3),
                      ret k (mk_id x)))))
  | EPrefixOp (_, op, e) ->
    cps' e throw
      (fun v -> 
        let x = new_name () in
        Bind (new_node (), 
              x, Op1 (Op1Prefix op, v),
              ret k (mk_id x)))
  | EInfixOp (_, op, e1, e2) -> 
    cps' e1 throw
      (fun v1 ->
        cps' e2 throw
          (fun v2 ->
            let x = new_name () in
            Bind (new_node (), 
                  x, Op2 (Op2Infix op, v1, v2),
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
  | EAssertTyp (_, t, EFunc (_, args, fi, body)) ->
    if fi.func_loop then
      let k' = new_name () in
      let throw' = new_name () in
      ret k (Lambda (k' :: throw' :: args, cps_exp body throw' (Jmp k')))
    else
      ret k (ExternalLambda t)
  | EAssertTyp (_, t, e) -> 
    cps' e throw (fun v ->
      let x = new_name () in
      Bind (new_node (), x,  AssertTyp (t, v),
            ret k (mk_id x))) 
  | EApp (_, func, args) -> 
    cps' func throw
      (fun f ->
        cps_exp_list args throw
          (fun vs -> 
            bind_cont k
              (fun k_id ->
                App (new_node (), f, (mk_id k_id) :: (mk_id throw) :: vs))))
  | EFunc (_, args, fi, body) -> 
    if fi.func_loop then
      let k' = new_name () in
      let throw' = new_name () in
      ret k (Lambda (k' :: throw' :: args, cps_exp body throw' (Jmp k')))
    else
      ret k (ExternalLambda TBot)
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
  | ERec (_, binds, body) ->
    Rec (new_node (), map cps_bind binds, cps_exp body throw k)
  | ESeq (_, e1, e2) ->
    cps' e1 throw (fun _ -> cps_exp e2 throw k)
  | ELabel (_, l, e) ->
    let r = new_name () in
    Bind (new_node (),
          l, Let (Lambda ([r], ret k (mk_id r))),
          cps_exp e throw (Jmp l))
  | EBreak (_, l, e) -> (* drops its own continuation *)
    cps_exp e throw (Jmp l)
  | EThrow (_, e) ->
    cps_exp e throw (Jmp throw)
  | ESubsumption (_, _, e) -> cps_exp e throw k
  | EDowncast (_, _, e) -> cps_exp e throw k
  | ETypecast (_, _, e) -> cps_exp e throw k
  | ETypAbs (_, _, _, e) -> cps_exp e throw k
  | ETypApp (_, e, _) -> cps_exp e throw k
  | ECheat (_, _, e) -> cps_exp e throw k
  | ETryCatch (p, body, exn, catch_body) ->
    let throw' = mk_name "throw-cont" in
    bind_cont k
      (fun cont ->
        Bind (new_node (),
              throw', Let (Lambda ([exn], cps_exp catch_body throw (Jmp cont))),
              cps_exp body throw' (Jmp cont)))
  | ETryFinally (_, e1, e2) -> (*TODO: make this not drop the finally *)
      cps_exp e1 throw k
  | EParen (_, e) -> cps_exp e throw k

and cps_bind ((name, t, e) : id * typ * exp) = match e with
  | EAssertTyp (_, _, e) -> cps_bind (name, t, e)
  | EFunc (_, args, fi, body) ->
    if fi.func_loop then
      let k = new_name () in
      let throw = new_name () in
      (name, Lambda (k :: throw :: args, cps_exp body throw (Jmp k)))
    else 
      (name, ExternalLambda t)
  | _ -> failwith "cps_bind : expected a function"

and cps_exp_list exps throw (k : cpsval list -> cpsexp) = match exps with
  | [] -> k []
  | e :: rest ->
    cps' e throw
      (fun v ->
        cps_exp_list rest throw
          (fun vs ->
            k (v :: vs)))

and cps' (e : exp) throw (f : cpsval -> cpsexp) = cps_exp e throw (Cont f)

let node_of_cpsexp (cpsexp : cpsexp) : node = match cpsexp with
  | App (n, _, _) -> n
  | Bind (n, _, _, _) -> n
  | If (n, _, _, _) -> n
  | Rec (n, _, _) -> n

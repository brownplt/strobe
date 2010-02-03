open Prelude
open Typedjs_syntax

type node = int

type edge_label = LNone | LTrue | LFalse

module NodeOrderedType = struct
  type t = node
  let compare = Pervasives.compare
end

module NodeMap = Map.Make (NodeOrderedType)

module NodeMapExt = MapExt.Make (NodeOrderedType) (NodeMap)

type 'a value =
    VId of 'a * id
  | VString of string
  | VNum of float
  | VInt of int
  | VRegexp of string * bool * bool
  | VBool of bool
  | VNull
  | VArray of 'a value list
  | VObject of (string * 'a value) list
  | VThis
  | VFunc of id list * typ * 'a anfexp
  | VUndefined

and 'a bind =
  | BValue of 'a value
  | BApp of 'a value * 'a value list
  | BBracket of 'a value * 'a value
  | BNew of 'a value * 'a value list
  | BPrefixOp of JavaScript_syntax.prefixOp * 'a value
  | BInfixOp of JavaScript_syntax.infixOp * 'a value * 'a value
  | BAssign of id * 'a value
  | BSetProp of 'a value * 'a value * 'a value
  | BIf of 'a value * 'a anfexp * 'a anfexp


and 'a anfexp =
    ALet of node * id * 'a bind * 'a anfexp
  | ARec of node * (id * 'a bind) list * 'a anfexp
  | ALabel of node * id * 'a anfexp
  | ABreak of node * id * 'a value
  | ATryCatch of node * 'a anfexp * id * 'a anfexp
  | ATryFinally of node * 'a anfexp * 'a anfexp
  | AThrow of node * 'a value
  | AValue of node * 'a value

let next_name = ref 0

let new_name () = 
  incr next_name;
  "%anf" ^ string_of_int (!next_name - 1)

let node : unit -> node =
  let next_node = ref 0 in
    fun () -> 
      incr next_node;
      !next_node - 1
      


let name_bind k loc v = 
  let x = new_name () in
    ALet (node (), x, v, k (VId (loc, x)))

let name_value v k = match v with
    VId (_, x) -> k x
  | v -> let x = new_name () in
      ALet (node (), x, BValue v, k x)

let rec to_anf_exps exps (k : 'a value list -> 'a anfexp) = match exps with
    [] -> k []
  | e :: es -> 
      to_anf e 
        (fun v -> 
           (to_anf_exps es 
              (fun vs -> 
                 k (v :: vs))))

and to_anf exp (k : 'a value -> 'a anfexp) = match exp with
    EString (_, s) -> k (VString s)
  | ERegexp (_, s, g, i) -> k (VRegexp (s, g, i))
  | ENum (_, x) -> k (VNum x)
  | EInt (_, n) -> k (VInt n)
  | EBool (_, b) -> k (VBool b)
  | ENull _ -> k VNull
  | EArray (_, es) -> to_anf_exps es (fun vs -> k (VArray vs))
  | EObject (_, props) -> to_anf_exps (map snd2 props)
      (fun vs -> k (VObject (List.combine (map fst2 props) vs)))
  | EUndefined _ -> k VUndefined
  | EThis _ -> k VThis
  | EId (p, x) ->  k (VId (p, x))
  | EBracket (p, e1, e2) ->
        to_anf e1 
          (fun v1 ->
             to_anf e2
               (fun v2 ->
                  name_bind k p (BBracket (v1, v2))))
  | ENew (p, c, args) -> 
      to_anf c
        (fun c_v ->
           to_anf_exps args
             (fun args_v -> 
                name_bind k p (BNew (c_v, args_v))))
  | EPrefixOp (p, op, e) -> 
      to_anf e 
        (fun v ->
           name_bind k p (BPrefixOp (op, v)))                  
  | EInfixOp (p, op, e1, e2) ->
      to_anf e1
        (fun v1 ->
           to_anf e2
             (fun v2 ->
                name_bind k p (BInfixOp (op, v1, v2))))
  | EIf (p, e1, e2, e3) ->
      to_anf e1
        (fun v1 ->
           name_bind k p
             (BIf (v1, 
                   to_anf e2 (fun v -> AValue (node (), v)),
                   to_anf e3 (fun v -> AValue (node (), v)))))
  | EAssign (p, LVar (_, x), e) ->
      to_anf e
        (fun v -> name_bind k p (BAssign (x, v)))
  | EAssign (p, LProp (_, e1, e2), e3) ->
      to_anf e1
        (fun v1 ->
           to_anf e2
             (fun v2 ->
                to_anf e3
                  (fun v3 ->
                     name_bind k p (BSetProp (v1, v2, v3)))))
  | EApp (p, fn, args) -> 
      to_anf fn
        (fun fn_v ->
           to_anf_exps args
             (fun args_v ->
                name_bind k p (BApp (fn_v, args_v))))
  | EFunc (_, args, t, e) ->
      k (VFunc (args, t, to_anf e (fun v -> AValue (node (), v))))
  | ELet (p, x, e1, e2) ->
      to_anf e1
        (fun v1 ->
           ALet (node (), x, BValue v1, to_anf e2 k))
  | ERec (binds, body) ->
(*      let bound_ids = map fst2 binds in
      let bound_bodies = to_anf_exps  *)
      let mk_bind (x, _, e) = 
        (match to_anf e (fun v -> AValue (node (), v)) with
             AValue (_, v) -> (x, BValue v) (* requires 2nd-order polymorphism *)
           | _ -> failwith "typedjs_anf.ml : error in ERec") in
        ARec (node (), map mk_bind binds, to_anf body k)
  | ESeq (p, e1, e2) ->
      to_anf e1
        (fun v1 ->
           let x = new_name () in
             ALet (node (), x, BValue v1, to_anf e2 k))
  | EBreak (p, x, e) ->
      to_anf e (fun v -> ABreak (node (), x, v)) (* ignore the continuation *)
  | ELabel (p, x, _, e) ->
      ALabel (node (), x, to_anf e k)
(*
  | ETryCatch of 'a * 'a exp * id * 'a exp
  | ETryFinally of 'a * 'a exp * 'a exp
  | EThrow of 'a * 'a exp
*)

let from_typedjs e = to_anf e (fun v -> AValue (node (), v))

(******************************************************************************)

open Format

type printer = formatter -> unit

let rec sep (lst : printer list) (fmt : formatter) : unit = match lst with
    x1 :: x2 :: xs' ->
      pp_open_box fmt 2;
      x1 fmt; 
      pp_close_box fmt ();
      pp_print_space fmt (); 
      sep (x2 :: xs') fmt
  | [x] -> 
      pp_open_box fmt 2;
      x fmt;
      pp_close_box fmt ()
  | [] -> ()

let text s fmt = pp_print_string fmt s

let enclose l r (lst : printer list) (fmt : formatter) = 
  pp_open_box fmt 2;
  pp_print_string fmt l;
  sep lst fmt;    
  pp_print_string fmt r;
  pp_close_box fmt ()

let parens = enclose "(" ")"

let braces = enclose "{" "}"

let brackets = enclose "[" "]"

let p_node n fmt =
  pp_print_string fmt "/";
  pp_print_int fmt n

let rec p_value v fmt = match v with
    VId (_, x) -> text x fmt
  | VString s -> text ("\"" ^ s ^ "\"") fmt
  | VNum x -> pp_print_float fmt x
  | VInt n -> pp_print_int fmt n
  | VRegexp (re, g, i) -> text ("/" ^ re ^ "/") fmt
  | VBool b -> pp_print_bool fmt b
  | VNull -> text "#null" fmt
  | VArray vs -> brackets (map p_value vs) fmt
  | VObject ps -> brackets (map p_prop ps) fmt
  | VThis -> text "#this" fmt
  | VFunc (args, _, e) ->
      parens [ text "fun"; parens (map text args); p_anfexp e ] fmt
  | VUndefined -> text "#undefined" fmt

and p_prop (s, v) =
  parens [ fun fmt ->
             pp_print_string fmt ("\"" ^ s ^ "\"");
             pp_print_space fmt ();
             pp_print_string fmt ":";
             pp_print_space fmt ();
             p_value v fmt ]

and p_bind b = match b with
    BValue v -> p_value v
  | BApp (v, vs) -> parens [ text "app"; p_value v; sep (map p_value vs) ]
  | BBracket (v1, v2) -> parens [ text "get-field"; p_value v1; p_value v2 ]
  | BNew (v, vs) -> parens [ text "new"; p_value v; sep (map p_value vs) ]
  | BPrefixOp (op, x) -> 
      parens [ text (JavaScript_pretty.render_prefixOp op); p_value x ]
  | BInfixOp (op, x, y) -> 
      parens [ text (JavaScript_pretty.render_infixOp op);
               p_value x; p_value y ]
  | BAssign (x, v) ->
      parens [ text "set!"; text x; p_value v ]
  | BSetProp (v1, v2, v3) ->
      parens [ text "set-field!"; p_value v1; p_value v2; p_value v3 ]
  | BIf (v1, e2, e3) ->
      parens [ text "if";  p_value v1; p_anfexp e2; p_anfexp e3 ]

and p_anfexp e = match e with
    ALet (n, x, b, e) ->
      parens [ text "let"; p_node n; text x; text "="; p_bind b; text "in";
               p_anfexp e ]
  | ARec (n, binds, e) ->
      parens [ text "rec"; p_node n; parens (map p_recbind binds); p_anfexp e ]
  | ALabel (n, x, e) ->
      parens [ text "label"; p_node n; text x; p_anfexp e ]
  | ABreak (n, x, v) ->
      parens [ text "break"; p_node n; text x; p_value v ]
  | AValue (n, v) ->
      parens [ text "return"; p_node n; p_value v ]


and p_recbind (x, b) =
  parens [ text x; p_bind b ]


let pretty_anfexp fmt e = p_anfexp e fmt

let print_anfexp e = pretty_anfexp std_formatter e; print_newline ()

(******************************************************************************)

let node_of_anfexp e = match e with
    ALet (n, _, _, _) -> n
  | ARec (n, _, _) -> n
  | ALabel (n, _, _) -> n
  | ABreak (n, _, _) -> n
  | ATryCatch (n, _, _, _) -> n
  | ATryFinally (n, _, _) -> n
  | AThrow (n, _) -> n
  | AValue (n, _) -> n

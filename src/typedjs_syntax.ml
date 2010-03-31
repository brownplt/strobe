open Prelude

exception Typ_error of pos * string

module RT = struct
  type t =
    | Number
    | String
    | Boolean
    | Function
    | Object
    | Undefined

  let compare = Pervasives.compare

  open FormatExt

  let pp v = match v with
    | Number -> text "number"
    | String -> text "string"
    | Boolean -> text "boolean"
    | Function -> text "function"
    | Object -> text "object"
    | Undefined -> text "undefined"

end

module RTSet = Set.Make (RT)
module RTSetExt = SetExt.Make (RTSet)

type constr = string

type typ = 
  | TConstr of constr * typ list
  | TUnion of typ * typ
  | TArrow of typ * typ list * typ
  | TObject of (id * typ) list
  | TRef of typ
  | TSource of typ
  | TSink of typ
  | TTop
  | TBot

type env_decl =
  | EnvClass of constr * constr option * (id * typ) list
  | EnvBind of id * typ

type annotation =
    ATyp of typ
  | AConstructor of typ 
  | AUpcast of typ
  | ADowncast of typ

(** Typed JavaScript expressions. Additional well-formedness criteria are
    inline. *)
type exp
  = EConst of pos * JavaScript_syntax.const
  | EArray of pos * exp list
  | EEmptyArray of pos * typ
  | EObject of pos * (string * exp) list
  | EThis of pos
  | EId of pos * id
  | EBracket of pos * exp * exp
  | ENew of pos * id * exp list
  | EPrefixOp of pos * JavaScript_syntax.prefixOp * exp
  | EInfixOp of pos * JavaScript_syntax.infixOp * exp * exp
  | EIf of pos * exp * exp * exp
  | EApp of pos * exp * exp list
  | EFunc of pos * id list * typ * exp
  | ELet of pos * id * exp * exp
  | ERec of (id * typ * exp) list * exp
  | ESeq of pos * exp * exp
  | ELabel of pos * id * typ * exp 
  | EBreak of pos * id * exp
  | ETryCatch of pos * exp * id * exp
  | ETryFinally of pos * exp * exp
  | EThrow of pos * exp
  | ETypecast of pos * RTSet.t * exp
  | ERef of pos * exp
  | EDeref of pos * exp
  | ESetRef of pos * exp * exp
  | ESubsumption of pos * typ * exp
  | EDowncast of pos * typ * exp

type constr_exp = { 
  constr_pos : pos;
  constr_name : id;
  constr_typ : typ;
  constr_args : id list;
  constr_inits : (id * exp) list;
  constr_exp : exp;
  constr_prototype : exp
}

type def =
    DEnd
  | DExp of exp * def
  | DLet of pos * id * exp * def
  | DRec of (id * typ * exp) list * def
  | DConstructor of constr_exp * def
  | DExternalMethod of pos * id * id * exp * def

(******************************************************************************)

module Exp = struct

  type t = exp

  let pos exp = match exp with
    | EConst (p, _) -> p
    | EArray (p, _) -> p
    | EObject (p, _) -> p
    | EThis p -> p
    | EId (p, _) -> p
    | EBracket (p, _, _) -> p
    | ENew (p, _, _) -> p
    | EPrefixOp (p, _, _) -> p
    | EInfixOp (p, _, _, _) -> p
    | EIf (p, _, _, _) -> p
    | EApp (p, _, _) -> p
    | EFunc (p, _, _, _) -> p
    | ELet (p, _, _, _) -> p
    | ERec (_, _) -> failwith "Exp.pos of ERec"
    | ESeq (p, _, _) -> p
    | ELabel (p, _, _, _) -> p
    | EBreak (p, _, _) -> p
    | ETryCatch (p, _, _, _) -> p
    | ETryFinally (p, _, _) -> p
    | EThrow (p, _) -> p
    | ETypecast (p, _, _) -> p
    | ERef (p, _) -> p
    | EDeref (p, _) -> p
    | ESetRef (p, _, _) -> p
    | ESubsumption (p, _, _) -> p
    | EDowncast (p, _, _) -> p
    | EEmptyArray (p, _) -> p

end

module Pretty = struct

  open Format
  open FormatExt

  let rec typ t  = match t with
    |  TTop -> text "Any"
    | TBot -> text "DoesNotReturn"
    | TUnion (t1, t2) -> horz [typ t1; text "+"; typ t2]
    | TArrow (_, arg_typs, r_typ) ->
        horz[ horz (intersperse (text "*") 
                      (map (fun at -> begin match at with
                              | TArrow _ -> parens (typ at)
                              | _ -> typ at 
                            end) arg_typs));
              text "->";
              typ r_typ ]
    | TConstr (s, []) -> text s
    | TConstr (s, ts) ->
        horz [ text s; 
               angles (horz (intersperse (text ",") (map typ ts))) ]
    | TObject fs ->
        let f (k, t) = horz [ text k; text ":"; typ t ] in
          braces (horz (intersperse (text ",") (map f fs)))
    | TRef s -> horz [ text "ref"; parens (typ s) ]
    | TSource s -> horz [ text "source"; parens (typ s) ]
    | TSink s -> horz [ text "sink"; parens (typ s) ]

  let rec exp e = match e with
    | EConst (_, c) -> JavaScript.Pretty.p_const c
    | EEmptyArray _ -> text "[ ]"
    | EArray (_, es) -> angles (horz (map exp es))
    | EObject (_, ps) -> brackets (vert (map prop ps))
    | EThis _ -> text "this"
    | EId (_, x) -> text x
    | EBracket (_, e1, e2) -> squish [ exp e1; brackets (exp e2) ]
    | ENew (_, c_id, args) ->
        parens (horz (text "new" :: text c_id :: map exp args))
    | EIf (_, e1, e2, e3) ->
        parens (vert [ horz [ text "if"; exp e1 ]; exp e2; exp e3 ])
    | EApp (_, f, args) -> parens (horz (exp f :: map exp args))
    | EFunc (_, args, t, body) ->
        parens (vert [ horz [ text "fun"; parens (horz (map text args)); 
                              text ":"; typ t ];
                       exp body])
    | ELet (_, x, bound, body) ->
        parens (vert [ horz [ text "let";
                              parens (vert (map bind [(x, bound)]))];
                       exp body ])
    | ERec (binds, body) ->
        parens (vert [ horz [ text "rec"; parens (vert (map rec_bind binds)) ];
                       exp body ])
    | ESeq (_, e1, e2) -> parens (vert [ sep [ text "seq"; exp e1 ]; exp e2 ])
    | ELabel (_, x, t, e) -> parens (vert [ text "label"; text x; exp e ])
    | EBreak (_, x, e) -> parens (vert [ text "break"; text x; exp e ])
    | ETryCatch (_, body, x, catch) ->
        parens (vert [ text "try"; exp body; 
                       parens (vert [ text "catch"; text x; exp body ])])
    | ETryFinally (_, body, finally) ->
        parens (vert [ text "try"; exp body;
                       parens (vert [ text "finally"; exp finally ]) ])
    | EThrow (_, e) -> parens (vert [ text "throw"; exp e ])
    | EPrefixOp (_, op, e) -> parens (vert [ JavaScript.Pretty.p_prefixOp op;
                                             exp e ])
    | EInfixOp (_, op, e1, e2) ->
        parens (vert [ JavaScript.Pretty.p_infixOp op; exp e1; exp e2 ])
    | ETypecast (_, t, e) ->
        parens (vert [ text "cast"; RTSetExt.p_set RT.pp t; exp e ])
    | ERef (_, e) -> parens (vert [ text "ref"; exp e ])
    | EDeref (_, e) -> parens (vert [ text "deref"; exp e ])
    | ESetRef (_, e1, e2) -> parens (vert [ text "set-ref!"; exp e1; exp e2 ])
    | ESubsumption (_, t, e) ->
        parens (vert [ text "upcast"; parens (typ t); exp e ])
    | EDowncast (_, t, e) ->
        parens (vert [ text "downcast"; parens (typ t); exp e ])

  and prop (s, e) =
    parens (vert [ text s; text ":"; exp e ])

  and bind (x, e) = 
    parens (vert [text x; text "="; exp e])

  and rec_bind (x, t, e) = 
    parens (vert [text x; text ":"; typ t; text "="; exp e])

  let constr (c : constr_exp) =
    parens (vert [ sep [ text "define-constructor"; text c.constr_name; 
                         parens (horz (map text c.constr_args));
                         text ":"; typ c.constr_typ ];
                   exp c.constr_exp ])
      

  let rec p_def (d : def) = match d with
    | DEnd -> fun fmt -> pp_print_newline fmt () 
      | DExp (e, d') -> vert [ exp e; p_def d' ]
      | DConstructor (c, d') -> vert [ constr c; p_def d' ]
      | DExternalMethod (p, cname, fname, e, d) -> 
          vert [ sep [ text (cname ^ ".prototype." ^ fname ^ " = ");
                       exp e; ];
                 p_def d ]
      | DLet  (_, x, e, d') ->
          vert [ parens (vert [ horz [ text "define"; text x ]; exp e ]);
                 p_def d' ]
      | DRec (binds, d') ->
          let p_bind (x, t, e) = brackets (horz [ text x; exp e ]) in
            vert [ parens (vert [ text "define-rec"; 
                                  parens (vert (map p_bind binds)) ]);
                   p_def d' ]

  let p_typ = typ

  let p_exp = exp

  let pp_typ ppf t = p_typ t ppf
   
end

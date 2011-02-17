open Prelude

exception Typ_error of pos * string

module RT = struct
  type t =
    | Num
    | Str
    | Bool
    | Function
    | Object
    | Undefined

  let compare = Pervasives.compare

  open FormatExt

  let pp v = match v with
    | Num -> text "number"
    | Str -> text "string"
    | Bool -> text "boolean"
    | Function -> text "function"
    | Object -> text "object"
    | Undefined -> text "undefined"

end

module RTSet = Set.Make (RT)
module RTSetExt = SetExt.Make (RTSet)

type constr = string

type prim =
  | Num
  | Int
  | Str
  | True
  | False
  | Undef
  | Null

type typ = 
  | TPrim of prim
  | TUnion of typ * typ
  | TIntersect of typ * typ
  | TArrow of typ list * typ
  | TObject of (id * typ) list
  | TRef of typ
  | TSource of typ
  | TSink of typ
  | TTop
  | TBot
  | TForall of id * typ * typ (** [TForall (a, s, t)] forall a <: s . t *)
  | TId of id
  | TField
  | TRec of id * typ 
  | TSyn of id (** type synonym *)

let typ_bool = TUnion (TPrim (True), TPrim (False))

type env_decl =
  | EnvClass of constr * constr option * typ
  | EnvBind of id * typ

type annotation =
    ATyp of typ
  | AConstructor of typ 
  | AUpcast of typ
  | ADowncast of typ
  | ATypAbs of id * typ
  | ATypApp of typ
  | AAssertTyp of typ
  | ACheat of typ

type ref_kind =
  | RefCell
  | SourceCell
  | SinkCell

(** Typed JavaScript expressions. Additional well-formedness criteria are
    inline. *)
type exp
  = EConst of pos * JavaScript_syntax.const
  | EBot of pos
  | EAssertTyp of pos * typ * exp
  | EArray of pos * exp list
  | EEmptyArray of pos * typ
  | EObject of pos * (string * exp) list
  | EId of pos * id
  | EBracket of pos * exp * exp
  | EUpdate of pos * exp * exp * exp
  | ENew of pos * id * exp list
  | EPrefixOp of pos * id * exp
  | EInfixOp of pos * id * exp * exp
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
  | ERef of pos * ref_kind * exp
  | EDeref of pos * exp
  | ESetRef of pos * exp * exp
  | ESubsumption of pos * typ * exp
  | EDowncast of pos * typ * exp
  | ETypAbs of pos * id * typ * exp 
  | ETypApp of pos * exp * typ
  | EForInIdx of pos
  | ECheat of pos * typ * exp

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

module Typ = struct

  let rec match_func_typ (typ : typ) : (typ list * typ) option = match typ with
    | TForall (_, _, t) -> match_func_typ t
    | TArrow (args, ret) -> Some (args, ret)
    | _ -> None

end

module Exp = struct

  type t = exp

  let pos exp = match exp with
    | EConst (p, _) -> p
    | EBot p -> p
    | EAssertTyp (p, _, _) -> p
    | EArray (p, _) -> p
    | EObject (p, _) -> p
    | EId (p, _) -> p
    | EBracket (p, _, _) -> p
    | EUpdate (p, _, _, _) -> p
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
    | ERef (p, _, _) -> p
    | EDeref (p, _) -> p
    | ESetRef (p, _, _) -> p
    | ESubsumption (p, _, _) -> p
    | EDowncast (p, _, _) -> p
    | EEmptyArray (p, _) -> p
    | ETypApp (p, _, _) -> p
    | ETypAbs (p, _, _, _) -> p
    | EForInIdx p -> p
    | ECheat (p, _, _) -> p
end

module Pretty = struct

  open Format
  open FormatExt

  let rec typ t  = match t with
    | TTop -> text "Any"
    | TBot -> text "DoesNotReturn"
    | TPrim p -> text begin match p with
       | Num -> "Num"
       | Int -> "Int"
       | True -> "True"
       | False -> "False"
       | Str -> "Str"
       | Null -> "Null"
       | Undef -> "Undef"
      end
    | TUnion (t1, t2) -> horz [typ t1; text "+"; typ t2]
    | TIntersect (t1, t2) -> horz [typ t1; text "&"; typ t2]
    | TArrow (tt::arg_typs, r_typ) ->
        horz[ brackets (typ tt);
              horz (intersperse (text "*") 
                      (map (fun at -> begin match at with
                              | TArrow _ -> parens (typ at)
                              | _ -> typ at 
                            end) arg_typs));
              text "->";
              typ r_typ ]
    | TArrow (arg_typs, r_typ) ->
        horz[ horz (intersperse (text "*") 
                      (map (fun at -> begin match at with
                              | TArrow _ -> parens (typ at)
                              | _ -> typ at 
                            end) arg_typs));
              text "->";
              typ r_typ ]
    | TObject fs ->
        let f (k, t) = horz [ text k; text ":"; typ t ] in
          braces (horz (intersperse (text ",") (map f fs)))
    | TRef s -> horz [ text "ref"; parens (typ s) ]
    | TSource s -> horz [ text "source"; parens (typ s) ]
    | TSink s -> horz [ text "sink"; parens (typ s) ]
    | TForall (x, s, t) -> 
        horz [ text "forall"; text x; text "<:"; typ s; text "."; typ t ]
    | TId x -> text x
    | TField -> text "field"
    | TRec (x, t) -> horz [ text x; text "."; typ t ]

  let rec exp e = match e with
    | EConst (_, c) -> JavaScript.Pretty.p_const c
    | EBot _ -> text "bot"
    | EAssertTyp (_, t, e) ->
        parens (vert [ text "assert-typ"; parens (typ t); exp e ])
    | EEmptyArray _ -> text "[ ]"
    | EArray (_, es) -> brackets (horz (map exp es))
    | EObject (_, ps) -> brackets (vert (map prop ps))
    | EId (_, x) -> text x
    | EBracket (_, e1, e2) -> squish [ exp e1; brackets (exp e2) ]
    | EUpdate (_, e1, e2, e3) -> 
        squish [ exp e1; 
                 brackets (squish [ exp e2; text ":="; exp e3])]
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
    | EPrefixOp (_, op, e) -> parens (vert [ text op; exp e ])
    | EInfixOp (_, op, e1, e2) -> parens (horz [ text op; exp e1; exp e2 ])
    | ETypecast (_, t, e) ->
        parens (vert [ text "cast"; RTSetExt.p_set RT.pp t; exp e ])
    | ERef (_, _, e) -> parens (horz [ text "ref"; exp e ])
    | EDeref (_, e) -> parens (horz [ text "deref"; exp e ])
    | ESetRef (_, e1, e2) -> parens (horz [ text "set-ref!"; exp e1; exp e2 ])
    | ESubsumption (_, t, e) ->
        parens (vert [ text "upcast"; parens (typ t); exp e ])
    | EDowncast (_, t, e) ->
        parens (vert [ text "downcast"; parens (typ t); exp e ])
    | ETypApp (_, e, t) -> parens (horz [ text "typ-app"; exp e; typ t ])
    | ETypAbs (_, x, t, e) -> 
        parens (horz [ text "typ-abs"; text x; text "<:"; typ t; exp e ])
    | EForInIdx _ -> text "for-in-idx"
    | ECheat (_, t, e) -> parens (horz [ text "cheat"; typ t; exp e ])

  and prop (s, e) =
    parens (horz [ text s; text ":"; exp e ])

  and bind (x, e) = 
    parens (horz [text x; exp e])

  and rec_bind (x, t, e) = 
    parens (horz [text x; text ":"; typ t; exp e])

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

let string_of_typ = FormatExt.to_string Pretty.p_typ



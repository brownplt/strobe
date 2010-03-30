open Prelude
open Typedjs_syntax
open JavaScript_pretty
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
  | TApp (s, []) -> text s
  | TApp (s, ts) ->
      horz [ text s; 
            angles (horz (intersperse (text ",") (map typ ts))) ]
  | TObject fs ->
      let f (k, t) = horz [ text k; text ":"; typ t ] in
        braces (horz (intersperse (text ",") (map f fs)))
  | TRef s -> horz [ text "ref"; parens (typ s) ]
  | TSource s -> horz [ text "source"; parens (typ s) ]
  | TSink s -> horz [ text "sink"; parens (typ s) ]

let rec exp e = match e with
  | EConst (_, c) -> JavaScript_pretty.p_const c
  | EArray (_, es) -> parens (horz (map exp es))
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
      parens (vert [ horz [ text "let"; parens (vert (map bind [(x, bound)]))];
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
  | EPrefixOp (_, op, e) ->  parens (vert [ text (render_prefixOp op); exp e ])
  | EInfixOp (_, op, e1, e2) ->
      parens (vert [ text (render_infixOp op); exp e1; exp e2 ])
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
          

let rec def (d : def) = match d with
  | DEnd -> fun fmt -> pp_print_newline fmt () 
  | DExp (e, d') -> vert [ exp e; def d' ]
  | DConstructor (c, d') -> vert [ constr c; def d' ]
  | DExternalMethod (p, cname, fname, e, d) -> 
      vert [ sep [ text (cname ^ ".prototype." ^ fname ^ " = ");
                   exp e; ];
             def d ]
  | DLet  (_, x, e, d') ->
      vert [ parens (vert [ horz [ text "define"; text x ]; exp e ]);
             def d' ]
  | DRec (binds, d') ->
      let p_bind (x, t, e) = brackets (horz [ text x; exp e ]) in
        vert [ parens (vert [ text "define-rec"; 
                              parens (vert (map p_bind binds)) ]);
               def d' ]

let pretty_exp fmt e = exp e fmt

let pretty_typ fmt t = typ t fmt

let pretty_def fmt d = def d fmt

let p_typ = typ

let p_exp = exp

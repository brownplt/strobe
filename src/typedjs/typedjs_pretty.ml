open Prelude
open Typedjs_syntax
open JavaScript_pretty
open Format
open FormatExt

let rec typ t  = match t with
    TTop -> text "Any"
  | TBot -> text "DoesNotReturn"
  | TUnion (t1, t2) -> horz [typ t1; text "+"; typ t2]
  | TArrow (_, arg_typs, r_typ) ->
      horz[ horz (intersperse (text "*") 
                    (map (fun at -> begin match at with
                              TArrow _ -> parens [ typ at ]
                            | _ -> typ at 
                          end) arg_typs));
            text "->";
            typ r_typ ]
  | TApp (s, []) -> text s
  | TApp (s, ts) ->
      horz [ text s; 
            angles [ horz (intersperse (text ",") (map typ ts))] ]
  | TObject fs ->
      let f (k, t) = horz [ text k; text ":"; typ t ] in
        braces (intersperse (text ",") (map f fs))
  | TRef s -> horz [ text "ref"; parens [ typ s ] ]
  | TSource s -> horz [ text "source"; parens [ typ s ] ]
  | TSink s -> horz [ text "sink"; parens [ typ s ] ]
  | TDom -> text "Dom"

    

let rec exp e fmt = match e with
    EConst (_, c) -> Exprjs_pretty.p_const c fmt
  | EArray (_, es) -> parens (map exp es) fmt
  | EObject (_, ps) -> brackets (map prop ps) fmt
  | EThis _ -> pp_print_string fmt "#this"
  | EId (_, x) -> text x fmt
  | EBracket (_, e1, e2) ->
      exp e1 fmt;
      brackets [exp e2] fmt
  | EUpdateField (_, e1, e2, e3) ->
      squish [ exp e1; brackets [ exp e2; text "="; exp e3 ] ] fmt
  | ENew (_, c_id, args) -> 
      parens (text "new" :: text c_id :: map exp args) fmt
  | EIf (_, e1, e2, e3) ->
      parens [text "if"; exp e1; exp e2; exp e3] fmt
  | EApp (_, f, args) ->
      parens (exp f :: map exp args) fmt
  | EFunc (_, args, t, body) ->
      parens [text "fun"; parens (map text args); text ":"; typ t; exp body] fmt
  | ELet (_, x, bound, body) ->
      parens [vert [ sep [ text "let"; parens (map bind [(x, bound)])] ;
                     exp body ] ] fmt
  | ERec (binds, body) ->
      parens [text "rec"; parens (map rec_bind binds); exp body] fmt
  | ESeq (_, e1, e2) ->
      parens [ vert [ sep [ text "seq"; exp e1 ]; exp e2 ] ] fmt
  | ELabel (_, x, t, e) ->
      parens [text "label"; text x; exp e] fmt
  | EBreak (_, x, e) ->
      parens [text "break"; text x; exp e] fmt
  | ETryCatch (_, body, x, catch) ->
      parens [text "try"; exp body; 
              parens [text "catch"; text x; exp body]]
        fmt
  | ETryFinally (_, body, finally) ->
      parens [text "try"; exp body; parens [text "finally"; exp finally]] fmt
  | EThrow (_, e) ->
      parens [text "throw"; exp e] fmt
  | EPrefixOp (_, op, e) ->
      parens [ text (render_prefixOp op); exp e] fmt
  | EInfixOp (_, op, e1, e2) ->
      parens [ text (render_infixOp op); exp e1; exp e2] fmt
  | ETypecast (_, t, e) ->
      parens [ text "cast"; RTSetExt.p_set RT.pp t; exp e ] fmt
  | ERef (_, e) ->
      parens [ text "ref"; exp e ] fmt
  | EDeref (_, e) ->
      parens [ text "deref"; exp e ] fmt
  | ESetRef (_, e1, e2) ->
      parens [ text "set-ref!"; exp e1; exp e2 ] fmt
  | ESubsumption (_, t, e) ->
      parens [ text "upcast"; parens [typ t]; exp e ] fmt
  | EDowncast (_, t, e) ->
      parens [ text "downcast"; parens [typ t]; exp e ] fmt
  | EParens (_, e) -> exp e fmt

and prop (s, e) =
  parens [ text s; text ":"; exp e ]

and bind (x, e) = 
  parens [text x; text "="; exp e]

and rec_bind (x, t, e) = 
  parens [text x; text ":"; typ t; text "="; exp e]

let constr (c : constr_exp) =
  parens [ vert [ sep [ text "define-constructor"; text c.constr_name; 
                        parens (map text c.constr_args);
                        text ":"; typ c.constr_typ ];
                  exp c.constr_exp ] ]
          

let rec def (d : def) = match d with
    DEnd -> fun fmt -> pp_print_newline fmt () 
  | DExp (e, d') -> vert [ exp e; def d' ]
  | DConstructor (c, d') -> vert [ constr c; def d' ]
  | DExternalMethod (p, cname, fname, e, d) -> 
      vert [ sep [ text (cname ^ ".prototype." ^ fname ^ " = ");
                   exp e; ];
             def d ]
  | DLet  (_, x, e, d') ->
      vert [ parens [ text "define"; text x; exp e ]; def d' ]
  | DRec (binds, d') ->
      let p_bind (x, t, e) = brackets [ text x; exp e ] in
      vert [ parens [ text "define-rec"; parens [ vert (map p_bind binds) ] ];
             def d' ]

let pretty_exp fmt e = exp e fmt

let pretty_typ fmt t = typ t fmt

let pretty_def fmt d = def d fmt

let p_typ = typ

let p_exp = exp

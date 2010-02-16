open Prelude
open Typedjs_syntax
open JavaScript_pretty
open Format
open FormatExt

let rec typ t  = match t with
    TTop -> text "Any"
  | TBot -> text "DoesNotReturn"
  | TUnion (t1, t2) -> sep [typ t1; text "+"; typ t2]
  | TArrow (_, arg_typs, r_typ) ->
      sep [ sep (intersperse (text "*") (map typ arg_typs));
            text "->";
            typ r_typ ]
  | TApp (s, []) -> text s
  | TApp (s, ts) ->
      sep [ text s; 
            angles [sep (intersperse (text ",") (map typ ts))] ]
  | TObject fs ->
      let f (k, t) = sep [ text k; text ":"; typ t ] in
        braces (intersperse (text ",") (map f fs))
  | TRef s -> sep [ text "mutable"; parens [ typ s ] ]

let rec exp e fmt = match e with
    EString (_, s) -> pp_print_string fmt ("\"" ^ s ^ "\"")
  | ERegexp (_, re, _, _) -> pp_print_string fmt ("/" ^ re ^ "/")
  | ENum (_, f) -> pp_print_float fmt f
  | EInt (_, n) -> pp_print_int fmt n
  | EBool (_, b) -> pp_print_bool fmt b
  | ENull _ -> pp_print_string fmt "#null"
  | EArray (_, es) -> parens (map exp es) fmt
  | EObject (_, ps) -> brackets (map prop ps) fmt
  | EThis _ -> pp_print_string fmt "#this"
  | EUndefined _ -> pp_print_string fmt "#undefined"
  | EId (_, x) -> text x fmt
  | EBracket (_, e1, e2) ->
      exp e1 fmt;
      brackets [exp e2] fmt
  | ENew (_, c, args) -> 
      parens (text "new" :: exp c :: map exp args) fmt
  | EIf (_, e1, e2, e3) ->
      parens [text "if"; exp e1; exp e2; exp e3] fmt
  | EApp (_, f, args) ->
      parens (exp f :: map exp args) fmt
  | EFunc (_, args, t, body) ->
      parens [text "fun"; parens (map text args); text ":"; typ t; exp body] fmt
  | ELet (_, x, bound, body) ->
      parens [text "let"; parens (map bind [(x, bound)]); exp body] fmt
  | ERec (binds, body) ->
      parens [text "rec"; parens (map rec_bind binds); exp body] fmt
  | ESeq (_, e1, e2) ->
      parens [text "seq"; exp e1; exp e2] fmt
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
  | EAssign (_, lv, e) ->
      parens [text "set!"; lvalue lv; exp e] fmt
  | ETypecast (_, t, e) ->
      parens [ text "cast"; exp e ] fmt

and lvalue lv fmt = match lv with
    LVar (_, x) -> text x fmt
  | LProp (_, e1, e2) -> 
      exp e1 fmt;
      brackets [exp e2] fmt

and prop (s, is_mutable, e) =
  parens [ text s; 
           text (if is_mutable then "mutable:" else ":");
           exp e ]

and bind (x, e) = 
  parens [text x; text "="; exp e]

and rec_bind (x, t, e) = 
  parens [text x; text ":"; typ t; text "="; exp e]

let def (d : def) = match d with
    DExp e -> exp e
  | DExternalField (_, c_name, f_name, e) ->
      sep [ text (sprintf "%s.prototype.%s" c_name f_name); text "="; exp e ]
  | DExternalMethods methods -> 
      let f (_, c_name, f_name, _, e) =
        sep [ text (sprintf "%s.prototype.%s" c_name f_name); text "="; exp e ]
      in vert (map f methods)

let pretty_runtime_typ (fmt : formatter) (rt : runtime_typ) = match rt with
    RTNumber -> pp_print_string fmt "number"
  | RTString -> pp_print_string fmt "string"
  | RTBoolean -> pp_print_string fmt "boolean"
  | RTObject -> pp_print_string fmt "object"
  | RTUndefined -> pp_print_string fmt "undefined"
  | RTFunction -> pp_print_string fmt "function"

let pretty_abs_value (fmt : formatter) (v : abs_value) = match v with
    AVType s -> RTSetExt.pretty fmt pretty_runtime_typ s
  | AVTypeof x -> fprintf fmt "typeof %s" x
  | AVString s -> pp_print_string fmt ("\"" ^ s ^ "\"")
  | AVTypeIs (x, s) -> 
      fprintf fmt "typeof %s === " x;
      RTSetExt.pretty fmt pretty_runtime_typ s

let pretty_exp fmt e = exp e fmt

let pretty_typ fmt t = typ t fmt

let pretty_def fmt d = def d fmt

let pretty_defs fmt defs =  vert (map def defs) fmt

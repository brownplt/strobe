open Prelude
open Exprjs_syntax
open JavaScript_pretty
open Format
open FormatExt

let rec expr e fmt = match e with
    StringExpr (_, s) -> pp_print_string fmt ("\"" ^ s ^ "\"")
  | RegexpExpr (_, re, _, _) -> pp_print_string fmt ("/" ^ re ^ "/")
  | NumExpr (_, f) -> pp_print_float fmt f
  | IntExpr (_, n) -> pp_print_int fmt n
  | BoolExpr (_, b) -> pp_print_bool fmt b
  | NullExpr _ -> pp_print_string fmt "#null"
  | ArrayExpr (_, es) -> parens (map expr es) fmt
  | ObjectExpr (_, ps) -> brackets (map prop ps) fmt
  | ThisExpr _ -> pp_print_string fmt "#this"
  | UndefinedExpr _ -> pp_print_string fmt "#undefined"
  | VarExpr (_, x) -> pp_print_string fmt x
  | BracketExpr (_, e1, e2) ->
      expr e1 fmt;
      brackets [expr e2] fmt
  | NewExpr (_, c, args) -> 
      parens (text "new" :: expr c :: map expr args) fmt
  | IfExpr (_, e1, e2, e3) ->
      parens [text "if"; expr e1; expr e2; expr e3] fmt
  | AppExpr (_, f, args) ->
      parens (expr f :: map expr args) fmt
  | FuncExpr (_, args, body) ->
      parens [text "fun"; parens (map text args); expr body] fmt
  | LetExpr (_, x, e1, e2) ->
      parens [text "let"; text x; text "="; expr e1; text "in"; expr e2] fmt
  | SeqExpr (_, e1, e2) ->
      parens [text "seq"; expr e1; expr e2] fmt
  | VarDeclExpr (_, x, e) ->
      text "var" fmt;
      pp_print_space fmt ();
      text x fmt;
      pp_print_space fmt ();
      text "=" fmt;
      pp_print_space fmt ();
      expr e fmt
  | WhileExpr (_, e1, e2) ->
      parens [text "while"; expr e1; expr e2] fmt
  | DoWhileExpr (_, e1, e2) ->
      parens [text "do-while"; expr e1; expr e2] fmt
  | LabelledExpr (_, x, e) ->
      parens [text "label"; text x; expr e] fmt
  | BreakExpr (_, x, e) ->
      parens [text "break"; text x; expr e] fmt
  | TryCatchExpr (_, body, x, catch) ->
      parens [text "try"; expr body; 
              parens [text "catch"; text x; expr body]]
        fmt
  | TryFinallyExpr (_, body, finally) ->
      parens [text "try"; expr body; parens [text "finally"; expr finally]] fmt
  | ForInExpr (_, x, obj, body) -> 
      parens [text "for"; text x; text "in"; expr obj; expr body] fmt
  | ThrowExpr (_, e) ->
      parens [text "throw"; expr e] fmt
  | FuncStmtExpr (_, f, args, body) ->
      parens [text "function"; text f; parens (map text args); expr body] fmt
  | PrefixExpr (_, op, e) ->
      parens [ text (render_prefixOp op); expr e] fmt
  | InfixExpr (_, op, e1, e2) ->
      parens [ text (render_infixOp op); expr e1; expr e2] fmt
  | AssignExpr (_, lv, e) ->
      parens [text "set"; lvalue lv; expr e] fmt

and lvalue lv fmt = match lv with
    VarLValue (_, x) -> text x fmt
  | PropLValue (_, e1, e2) -> 
      expr e1 fmt;
      brackets [expr e2] fmt

and prop (_, s, e) =  parens [ text s; text ":"; expr e ]

let pretty_expr fmt e = expr e fmt

let print_expr e = expr e std_formatter; print_newline ()

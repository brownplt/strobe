open Prelude
open Typedjs_syntax
open Exprjs_syntax
open Typedjs_types
open Typedjs_env
open Typedjs_tc_util
open Typedjs_fromExpr
open Exprjs_syntax

module H = Hashtbl
module S = JavaScript_syntax

let rec upcast_map e = 
  let adcast = "upcast 'Ad" in
  let ad = "'Ad" in
  let objcast = "obj* 'AdObj" in
    match e with
      | ConstExpr (p, _) -> HintExpr (p, adcast, e)
      | ArrayExpr (p, []) ->
          HintExpr (p, ad, ArrayExpr (p, []))
      | ArrayExpr (p, es) ->
          HintExpr (p, adcast, ArrayExpr (p, map upcast_map es))
      | ObjectExpr (p, flds) ->
          let new_flds = map (fun (p, s, e') -> (p, s, (upcast_map e'))) flds in
            HintExpr (p, objcast, ObjectExpr (p, new_flds))
      | ThisExpr (p) -> HintExpr (p, adcast, e)
      | VarExpr (p, _) -> HintExpr (p, adcast, e)
      | IdExpr (p, _) -> HintExpr (p, adcast, e)
      | BracketExpr (p, o, f) -> 
          HintExpr (p, adcast, BracketExpr (p, upcast_map o, upcast_map f))
      | NewExpr (p, e', es) ->
          HintExpr (p, objcast, NewExpr (p, upcast_map e', map upcast_map es))
      | PrefixExpr (p, op, e') ->
          HintExpr (p, adcast, PrefixExpr (p, op, upcast_map e'))
      | InfixExpr (p, op, e1, e2) ->
          HintExpr (p, adcast, InfixExpr (p, op, upcast_map e1, upcast_map e2))
      | IfExpr (p, c, thn, els) ->
          HintExpr (p, adcast, IfExpr (p, upcast_map c,
                                       upcast_map thn,
                                       upcast_map els))
      | AssignExpr (p, x, e') -> 
          HintExpr (p, adcast, AssignExpr (p, x, upcast_map e'))
      | AppExpr (p, f, es) ->
          HintExpr (p, adcast, AppExpr (p, upcast_map f, map upcast_map es))
      | FuncExpr (p, xs, e') ->
          HintExpr (p, objcast, FuncExpr (p, xs, upcast_map e'))
      | LetExpr (p, x, e1, e2) ->
          HintExpr (p, adcast, LetExpr (p, x, upcast_map e1, upcast_map e2))
      | SeqExpr (p, e1, e2) ->
          HintExpr (p, adcast, SeqExpr (p, upcast_map e1, upcast_map e2))
      | WhileExpr (p, e1, e2) ->
          HintExpr (p, adcast, WhileExpr (p, upcast_map e1, upcast_map e2))
      | DoWhileExpr (p, e1, e2) ->
          HintExpr (p, adcast, DoWhileExpr (p, upcast_map e1, upcast_map e2))
      | LabelledExpr (p, x, e') ->
          HintExpr (p, adcast, LabelledExpr (p, x, upcast_map e'))
      | BreakExpr (p, x, e') ->
          HintExpr (p, adcast, BreakExpr (p, x, upcast_map e'))
      | ForInExpr (p, x, e1, e2) ->
          HintExpr (p, adcast, ForInExpr (p, x, upcast_map e1, upcast_map e2))
      | VarDeclExpr (p, x, e') ->
          VarDeclExpr (p, x, upcast_map e')
      | TryCatchExpr (p, e1, x, e2) ->
          HintExpr (p, adcast, 
                    TryCatchExpr (p, upcast_map e1, x, upcast_map e2))
      | TryFinallyExpr (p, e1, e2) ->
          HintExpr (p, adcast, 
                    TryFinallyExpr (p, upcast_map e1, upcast_map e2))
      | ThrowExpr (p, e') ->
          HintExpr (p, adcast,
                    ThrowExpr (p, upcast_map e'))
      | FuncStmtExpr (p, x, xs, e') ->
          HintExpr (p, objcast,
                    FuncStmtExpr (p, x, xs, upcast_map e'))
      | HintExpr (p, s, e') ->
          HintExpr (p, adcast, HintExpr (p, s, upcast_map e'))

let lint_annotate = upcast_map
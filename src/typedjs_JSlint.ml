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

let globals = ["ADSAFE"; "Math"]

let mk_fun_annot n =
  let ads = List.fold_right (fun _ s -> "'Ad *" ^ s) (iota n) "'Ad ..." in
    "['Ad + HTMLWindow] " ^ ads ^ " -> 'Ad"

let rec upcast_map e = 
  let adcast = "upcast 'Ad" in
  let ad = "'Ad" in
  let objcast = "obj* 'AdObj" in
    match e with
      | VarExpr (p, id) when List.mem id globals -> e
      | BracketExpr (p, VarExpr (p2, "ADSAFE"), 
                     ConstExpr (p', S.CString s)) -> e
(** Special case for ADSAFE.go *)
      | AppExpr (p, BracketExpr (p'', VarExpr (p', "ADSAFE"), 
                                 ConstExpr (p''', S.CString "go")), e::es) -> 
          AppExpr (p, BracketExpr (p'', VarExpr (p', "ADSAFE"), 
                                   ConstExpr (p''', S.CString "go")), 
                   e::(map upcast_map es)) 
      | FuncExpr (p, xs, e') ->
          HintExpr (p, adcast,
                    HintExpr (p, objcast, 
                              HintExpr (p, mk_fun_annot (List.length xs), 
                                        FuncExpr (p, xs, upcast_map e'))))
      | FuncStmtExpr (p, x, xs, e') ->
          HintExpr (p, adcast,
                    HintExpr (p, objcast,
                              HintExpr (p, mk_fun_annot (List.length xs),
                                        FuncStmtExpr (p, x, xs, upcast_map e'))))
      | BracketExpr (p, o, ConstExpr (p', S.CString s)) -> 
          HintExpr (p, adcast, BracketExpr (p, upcast_map o, ConstExpr (p', S.CString s)))
      | BracketExpr (p, o, ConstExpr (p', S.CInt n)) -> 
          HintExpr (p, adcast, BracketExpr (p, upcast_map o, ConstExpr (p', S.CInt n)))
      | ConstExpr (p, _) -> HintExpr (p, adcast, e)
      | ArrayExpr (p, []) ->
          HintExpr (p, adcast, HintExpr (p, objcast, HintExpr (p, ad, ArrayExpr (p, []))))
      | ArrayExpr (p, es) ->
          HintExpr (p, adcast, HintExpr (p, objcast, ArrayExpr (p, map upcast_map es)))
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
      | PrefixExpr (p, "prefix:+", _) -> e
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
      | LetExpr (p, x, e1, e2) ->
          HintExpr (p, adcast, LetExpr (p, x, upcast_map e1, upcast_map e2))
      | SeqExpr (p, e1, (ConstExpr (_, S.CUndefined) as undef)) -> 
          SeqExpr (p, upcast_map e1, undef)
      | SeqExpr (p, e1, e2) ->
          SeqExpr (p, upcast_map e1, upcast_map e2)
      | WhileExpr (p, e1, e2) ->
          WhileExpr (p, upcast_map e1, upcast_map e2)
      | DoWhileExpr (p, e1, e2) ->
          DoWhileExpr (p, upcast_map e1, upcast_map e2)
      | LabelledExpr (p, x, e') ->
          LabelledExpr (p, x, upcast_map e')
      | BreakExpr (p, x, ConstExpr (_, S.CUndefined)) -> e
      | BreakExpr (p, x, e') ->
          BreakExpr (p, x, upcast_map e')
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
      | HintExpr (p, s, e') ->
          HintExpr (p, adcast, HintExpr (p, s, upcast_map e'))

let lint_annotate = upcast_map

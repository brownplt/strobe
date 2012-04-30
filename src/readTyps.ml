open Prelude
open Typedjs_syntax
open Lexing
open Typedjs_parser
open Typedjs_lexer
open JavaScript_syntax
module H = Hashtbl

type ann = (Prelude.Pos.t * Typedjs_syntax.annotation) list

type comments = (Prelude.Pos.t * string) list

type typ_db = (Prelude.Pos.t, Typedjs_syntax.annotation) H.t

let is_typedecl (_, comment) =
  String.length comment > 1 && comment.[0] = ':' && comment.[1] = ':'

let is_annotation (_, comment) =
  String.length comment > 0 && comment.[0] = ':'

let parse_annotation ((pos, end_p, _), comment) =
  let lexbuf = from_string (String.sub comment 1 (String.length comment - 1)) in
    lexbuf.lex_start_p <- pos;
    lexbuf.lex_curr_p <- pos;
    try
      (Pos.real (pos, end_p), typ_ann token lexbuf)
    with
      | Failure "lexing: empty token" ->
          failwith (sprintf "error lexing annotation at %s"
                   (Pos.rangeToString lexbuf.lex_curr_p lexbuf.lex_curr_p))
      | Error ->
          failwith (sprintf "error parsing annotation at %s"
                   (Pos.rangeToString lexbuf.lex_curr_p lexbuf.lex_curr_p))

let parse_typedecl ((pos, _, _), comment) =
  let lexbuf = from_string (String.sub comment 2 (String.length comment - 2)) in
  lexbuf.lex_start_p <- pos;
  lexbuf.lex_curr_p <- pos;
  try
    env token lexbuf
  with
  | Failure "lexing: empty token" ->
    failwith (sprintf "error lexing annotation at %s"
                (Pos.rangeToString lexbuf.lex_curr_p lexbuf.lex_curr_p))
  | Error ->
    failwith (sprintf "error parsing annotation at %s, annotation was\n%s"
                (Pos.rangeToString lexbuf.lex_curr_p lexbuf.lex_curr_p) comment)

let new_decls (comments : (Pos.t * string) list) : env_decl list =
  List.concat (ListExt.filter_map (fun c -> if is_typedecl c then Some (parse_typedecl c) else None) comments)
    

let annotations_of_comments (comments : (Pos.t * string) list) : (Pos.t * annotation) list = 
  let (_, not_typedecls) = List.partition is_typedecl comments in
  map parse_annotation (List.filter is_annotation not_typedecls)

let pos_before (p1, _, _) (p2, _, _) =
  p1.pos_cnum < p2.pos_cnum

let pos_near p1 p2 =
  true (* TODO(arjun): figure out nearness *)

(* [assoc_annotations js_ast ann_lst] produces a map from expressions to
   the annotation on them. *)
let assoc_annotations (js_ast : prog) ann_lst =
  let tbl : typ_db = H.create 200 in
  let rec stmt ann s = match s with
    | BlockStmt (_, stmts) -> fold_left stmt ann stmts
    | EmptyStmt _ -> ann
    | ExprStmt e -> expr ann e
    | IfStmt (_, e1, s2, s3) -> stmt (stmt (expr ann e1) s2) s3
    | IfSingleStmt (_, e1, s2) -> stmt (expr ann e1) s2
    | SwitchStmt (_, e, cases) -> fold_left caseClause (expr ann e) cases
    | WhileStmt (_, e, s) -> stmt (expr ann e) s
    | DoWhileStmt (_, s1, e2) -> stmt (expr ann e2) s1
    | BreakStmt _ -> ann
    | BreakToStmt _ -> ann
    | ContinueStmt _ -> ann
    | ContinueToStmt _ -> ann
    | LabelledStmt (_, _, s) -> stmt ann s
    | ForInStmt (_, fii, e, s) -> stmt (expr (forInInit ann fii) e) s
    | ForStmt (_, fi, e1, e2, s3) -> 
        stmt (expr (expr (forInit ann fi) e1) e2) s3
    | TryStmt (_, s1, catches, s3) ->
        stmt (fold_left catch (stmt ann s1) catches) s3
    | ThrowStmt (_, e) -> expr ann e
    | ReturnStmt (_, e) -> expr ann e
    | WithStmt (_, e, s) -> stmt (expr ann e) s
    | VarDeclStmt (_, decls) -> fold_left varDecl ann decls
      (* TODO(arjun): also annotatable *)
    | FuncStmt (stmt_p, f, args, s) -> match ann with
      | ((p, a) :: rest) ->
        (* above statement, or between arguments and opening brace *)
        let isSynth = Pos.isSynthetic stmt_p in
        if (not isSynth &&
              ((pos_before p stmt_p) ||
                  (pos_before stmt_p p && pos_before p (pos_stmt s)))) then
          if pos_near stmt_p p then
            begin
              H.add tbl stmt_p a;
              stmt rest s
            end
          else
            failwith "annotation is too far away"
        else
          stmt ann s
     | [] -> stmt ann s
  and varDecl ann vd = match vd with
    | VarDeclNoInit (_, x) -> ann (* TODO: annotate *)
    | VarDecl (_, x, e) -> expr ann e (* TODO: annotate *)
  and forInit ann fi = match fi with
    | NoForInit _ -> ann
    | VarForInit decls -> fold_left varDecl ann decls
    | ExprForInit e -> expr ann e
  and catch ann c = match c with
    | CatchClause (_, x, s) -> stmt ann s
  and forInInit ann fii = match fii with
    | VarForInInit (_, x) -> ann
    | NoVarForInInit (_, x) -> ann
  and caseClause ann clause = match clause with
    | CaseClause (_, e, s) -> stmt (expr ann e) s
    | CaseDefault (_, s) -> stmt ann s
  and lvalue ann lval = match lval with
    | VarLValue (_, x) -> ann
    | DotLValue (_, e, x) -> expr ann e
    | BracketLValue (_, e1, e2) -> expr (expr ann e1) e2
  and expr_rec (ann : ann) (e : expr) = match e with
    | ConstExpr _ -> ann
    | ArrayExpr (_, es) -> fold_left expr ann es
    | ObjectExpr (_, props) -> 
        fold_left (fun ann (_, _, e) -> expr ann e) ann props
    | ThisExpr _ -> ann
    | VarExpr _ -> ann
    | DotExpr (_, e, _) -> expr ann e
    | BracketExpr (_, e1, e2) -> expr (expr ann e1) e2
    | NewExpr (_, e1, es) -> fold_left expr ann (e1 :: es)
    | PrefixExpr (_, _, e) -> expr ann e
    | UnaryAssignExpr (_, _, lval) -> lvalue ann lval
    | InfixExpr (_, _, e1, e2) -> expr (expr ann e1) e2
    | IfExpr (_, e1, e2, e3) -> expr (expr (expr ann e1) e2) e3
    | AssignExpr (_, _, lv, e) -> expr (lvalue ann lv) e
    | ParenExpr (_, e) -> expr ann e
    | ListExpr (_, e1, e2) -> expr (expr ann e1) e2
    | CallExpr (_, e, es) -> fold_left expr ann (e :: es)
    | FuncExpr (_, _, s)
    | NamedFuncExpr (_, _, _, s) -> match ann with
      | ((p, a) :: rest) ->
        let expr_p = pos_expr e in
        let isSynth = Pos.isSynthetic expr_p in
        if (not isSynth &&
               (pos_before expr_p p && pos_before p (pos_stmt s))) then
          if pos_near expr_p p then
            begin
              H.add tbl expr_p a;
              stmt rest s
            end
          else
            failwith "annotation is too far away"
        else
          stmt ann s
     | [] -> stmt ann s
  and expr (ann : ann) (e : expr) : ann = match ann with
    | [] -> ann
    | ((p, a) :: rest) -> 
      let expr_p = pos_expr e in
      if pos_before p expr_p && pos_near p expr_p then
        begin
          H.add tbl expr_p a;
          expr_rec rest e
        end
      else if not (pos_before p expr_p) then
        (* the annotation is after the expression, recur *)
        expr_rec ann e
      else 
        failwith "annotation is too far away"
  in match js_ast with
  | Prog (_, stmts) -> match fold_left stmt ann_lst stmts with
    | [] -> tbl
    | _ -> failwith "extra annotations"

let read_typs (js_ast : prog) (comments : (Pos.t * string) list) : typ_db =
  assoc_annotations js_ast (annotations_of_comments comments)


let get_annotation typ_db p = 
  try 
    let ann = H.find typ_db p in
    H.remove typ_db p;
    Some ann
  with Not_found -> None

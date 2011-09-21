open Prelude
open Typedjs_syntax
open Lexing
open Typedjs_parser
open Typedjs_lexer
open JavaScript_syntax
module H = Hashtbl

type ann = (pos * Typedjs_syntax.annotation) list

type comments = (Prelude.pos * string) list

type typ_db = (pos, Typedjs_syntax.annotation) H.t

let is_annotation (_, comment) =
  String.length comment > 0 && String.get comment 0 = ':'

let parse_annotation ((pos, end_p), comment) =
  let lexbuf = from_string (String.sub comment 1 (String.length comment - 1)) in
    lexbuf.lex_start_p <- pos;
    lexbuf.lex_curr_p <- pos;
    try
      ((pos, end_p), typ_ann token lexbuf)
    with
      | Failure "lexing: empty token" ->
          failwith (sprintf "error lexing annotation at %s"
                   (string_of_position (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))
      | Error ->
          failwith (sprintf "error parsing annotation at %s"
                   (string_of_position (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))

let annotations_of_comments (comments : (pos * string) list)
      : (pos * annotation) list = 
  map parse_annotation (List.filter is_annotation comments)

let pos_before (p1, _) (p2, _) = p1.pos_cnum < p2.pos_cnum

let pos_near (p1, _) (p2, _) = 
  true (* TODO(arjun): figure out nearness *)

let expr_pos (e : expr) : pos = match e with
  | ConstExpr (p, _) -> p
  | ArrayExpr (p, _) -> p
  | ObjectExpr (p, _) -> p
  | ThisExpr p -> p
  | VarExpr (p, _) -> p
  | DotExpr (p, _, _) -> p
  | BracketExpr (p, _, _) -> p
  | NewExpr (p, _, _) -> p
  | PrefixExpr (p, _, _) -> p
  | UnaryAssignExpr (p, _, _) -> p
  | InfixExpr (p, _, _, _) -> p
  | IfExpr (p, _, _, _) -> p
  | AssignExpr (p, _, _, _) -> p
  | ParenExpr (p, _) -> p
  | ListExpr (p, _, _) -> p
  | CallExpr (p, _, _) -> p
  | FuncExpr (p, _, _) -> p
  | NamedFuncExpr (p, _, _, _) -> p
    
let stmt_pos (s : stmt) : pos = match s with
  | BlockStmt (p, stmts) -> p
  | EmptyStmt p -> p
  | ExprStmt e -> expr_pos e
  | IfStmt (p, e1, s2, s3) -> p
  | IfSingleStmt (p, e1, s2) -> p
  | SwitchStmt (p, e, cases) -> p
  | WhileStmt (p, e, s) -> p
  | DoWhileStmt (p, s1, e2) -> p
  | BreakStmt p -> p
  | BreakToStmt (p, _) -> p
  | ContinueStmt p -> p
  | ContinueToStmt (p, _) -> p
  | LabelledStmt (p, _, s) -> p
  | ForInStmt (p, fii, e, s) -> p
  | ForStmt (p, fi, e1, e2, s3) -> p
  | TryStmt (p, s1, catches, s3) -> p
  | ThrowStmt (p, e) -> p
  | ReturnStmt (p, e) -> p
  | WithStmt (e, s) -> expr_pos e (* TODO(arjun): skips start of With *)
  | VarDeclStmt (p, decls) -> p
  | FuncStmt (p, f, args, s) -> p

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
    | WithStmt (e, s) -> stmt (expr ann e) s
    | VarDeclStmt (_, decls) -> fold_left varDecl ann decls
      (* TODO(arjun): also annotatable *)
    | FuncStmt (stmt_p, f, args, s) -> match ann with
      | ((p, a) :: rest) ->
        if pos_before stmt_p p && pos_before p (stmt_pos s) then
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
        let expr_p = expr_pos e in
        if pos_before expr_p p && pos_before p (stmt_pos s) then
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
      let expr_p = expr_pos e in
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

let read_typs (js_ast : prog) (comments : (pos * string) list) : typ_db =
  assoc_annotations js_ast (annotations_of_comments comments)

let get_annotation typ_db p = 
  try 
    let ann = H.find typ_db p in
    H.remove typ_db p;
    Some ann
  with Not_found -> None

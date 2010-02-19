open JavaScript_syntax
open Prelude
open Format
open FormatExt

let prefixOp (op : prefixOp) = match op with
    PrefixLNot -> text "!"
  | PrefixBNot -> text "~"
  | PrefixPlus -> text "+"
  | PrefixMinus -> text "-"
  | PrefixTypeof -> text "typeof"
  | PrefixVoid -> text "void"
  | PrefixDelete -> text "delete"


let unaryAssignOp (op : unaryAssignOp) = match op with
    PrefixInc -> text "++"
  | PrefixDec -> text "--"
  | PostfixInc -> text "++"
  | PostfixDec -> text "--"

let prefix_unaryAssignOp op = match op with
    PrefixInc -> true
  | PrefixDec -> true
  | PostfixInc -> false
  | PostfixDec -> false


let infixOp (op : infixOp) = match op with
    OpLT -> text "<"
  | OpLEq -> text "<="
  | OpGT -> text ">"
  | OpGEq -> text ">="
  | OpIn -> text "in"
  | OpInstanceof -> text "instanceof"
  | OpEq -> text "=="
  | OpNEq -> text "!="
  | OpStrictEq -> text "==="
  | OpStrictNEq -> text "!=="
  | OpLAnd -> text "&&"
  | OpLOr -> text "||"
  | OpMul -> text "*"
  | OpDiv -> text "/"
  | OpMod -> text "%"
  | OpSub -> text "-"
  | OpLShift -> text "<<"
  | OpSpRShift -> text ">>>"
  | OpZfRShift -> text ">>"
  | OpBAnd -> text "&"
  | OpBXor -> text "^"
  | OpBOr -> text "|"
  | OpAdd -> text "+"

let assignOp (op : assignOp) = match op with
     OpAssign -> text "="
  | OpAssignAdd -> text "+="
  | OpAssignSub -> text "-="
  | OpAssignMul -> text "*="
  | OpAssignDiv -> text "/="
  | OpAssignMod -> text "%="
  | OpAssignLShift -> text "<<="
  | OpAssignSpRShift -> text ">>>="
  | OpAssignZfRShift -> text ">>="
  | OpAssignBAnd -> text "&="
  | OpAssignBXor -> text "^="
  | OpAssignBOr -> text "|="


let string s = text ("\"" ^ s ^ "\"") (* TODO: fix escapes *)

let rec commas (ps : printer list) = match ps with
    [] -> []
  | [p] -> [p]
  | p1 :: p2 :: ps -> 
      (fun fmt -> 
         pp_open_vbox fmt 0 ;
         p1 fmt;
         pp_print_string fmt ",";
         pp_close_box fmt ()) :: commas (p2 :: ps)

let prop (p : prop) = match p with
    PropId x -> text x
  | PropString s -> string s
  | PropNum n -> int n


let rec varDecl decl = match decl with
    VarDeclNoInit (_,x) -> text x
  | VarDecl (_,x,e) -> sep [text x; text "="; expr e]


and caseClause clause = match clause with
    CaseClause (_,e,s) -> sep [expr e; text ":"; stmt s]
  | CaseDefault (_,s) -> sep [text "default:"; stmt s]

and block s = match s with
    BlockStmt _ -> stmt s
  | _ -> vert [ text "{"; nest (stmt s); text "}" ]

and paren_exp e = match e with
    ParenExpr _ -> expr e
  | _ -> sep [ text "("; expr e; text ")" ]


and for_init fi = match fi with
    NoForInit -> text ""
  | VarForInit decls -> sep [ text "var"; 
                              vert (commas (List.map varDecl decls)) ]
  | ExprForInit e -> expr e


and for_in_init fii = match fii with
    VarForInInit (_,x) -> sep [text "var"; text x]
  | NoVarForInInit (_,x) -> text x


and lvalue lv = match lv with
    VarLValue (_,x) -> text x
  | DotLValue (_,e,x) -> sep [expr e; text "."; text x]
  | BracketLValue (_,e1,e2) -> sep [expr e1; brackets [expr e2]]


and catch clause = match clause with
    CatchClause (_,x,s) -> 
      vert [ sep [ text "catch"; parens [text x] ]; block s ]


and expr e = match e with
    StringExpr (_,s) -> string s
  | RegexpExpr (p,s,g,i) -> string s (* TODO: fixme *)
  | NumExpr (_,f) -> text (string_of_float f)
  | IntExpr (_,n) -> int n
  | BoolExpr (_,true) -> text "true"
  | BoolExpr (_,false) -> text "false"
  | NullExpr _ -> text "null"
  | ArrayExpr (_,es) -> brackets (List.map expr es)
  | ObjectExpr (_,ps) -> 
      let f (_, p, e) = sep [prop p; text ":"; expr e]
      in vert [ text "{"; nest (vert (map f ps)); text "}" ]
  | ThisExpr _ -> text "this"
  | VarExpr (_,x) -> text x
  | DotExpr (_,e,x) -> sep [expr e; text "."; text x]
  | BracketExpr (_,e1,e2) -> sep [expr e1; text "["; expr e2; text "]"]
  | NewExpr (_,constr,args) -> 
      sep [text "new "; expr constr; parens (commas (List.map expr args)) ]
  | PrefixExpr (_,op,e) -> sep [prefixOp op; expr e]
  | UnaryAssignExpr (_, op, lv) -> 
      if prefix_unaryAssignOp op
      then sep [ unaryAssignOp op; lvalue lv ]
      else sep [ lvalue lv; unaryAssignOp op ]
  | InfixExpr (_,op,e1,e2) ->
      sep [expr e1; infixOp op; expr e2]
  | IfExpr (_,e1,e2,e3) ->
      sep [expr e1; text "?"; expr e2; text ":"; expr e3]
  | AssignExpr (_,op,lv,e) -> sep [lvalue lv; assignOp op; expr e]
  | ParenExpr (_,e) -> sep [ parens [ expr e ] ]
  | ListExpr (_,e1,e2) -> sep (commas [expr e1; expr e2 ])
  | CallExpr (_,func,args) ->
      sep [ expr func; parens (commas (map expr args)) ]
  | FuncExpr (_,args,body) ->
      vert [ sep [ text "function"; parens (commas (map text args)) ];
             stmt body ]
  | NamedFuncExpr (_,name,args,body) ->
      vert [ sep [ text "function"; text name; 
                   parens (commas (map text args)) ];
             stmt body ]
  | UndefinedExpr _ -> text ""

and stmt s = match s with
   BlockStmt (_,ss) -> 
     vert [ text "{"; nest (vert (List.map stmt ss)); text "}" ]
  | EmptyStmt _ -> text ";"
  | ExprStmt e -> sep [ expr e; text ";" ]
  | IfStmt (_,e,s1,s2) ->
      vert [ sep [ text "if"; paren_exp e ]; stmt s1; text "else"; stmt s2 ]
  | IfSingleStmt (_,e,s1) -> vert [ sep [text "if"; paren_exp e ]; stmt s1 ]
  | SwitchStmt (_,e,clauses) ->
      vert [ sep [ text "switch"; paren_exp e ];
             braces (List.map caseClause clauses) ]
  | WhileStmt (_,e,s) -> vert [ sep [ text "while"; paren_exp e ]; stmt s ]
  | DoWhileStmt (_,s,e) -> 
      sep [text "do"; stmt s; text "while"; paren_exp e]
  | BreakStmt _ -> text "break;"
  | BreakToStmt (_,x) -> text ("break " ^ x ^ ";") 
  | ContinueStmt _ -> text "continue;"
  | ContinueToStmt  (_,x) -> text ("continue " ^ x ^ ";")
  | LabelledStmt (_,x,s) -> sep [text (x ^ ":"); stmt s]
  | ForInStmt (_,fii,e,s) ->
      vert [ sep [ text "for"; parens [ for_in_init fii; text "in "; expr e] ];
              block s ]
  | ForStmt (_,fi,e1,e2,s) ->
     vert [ sep  [text "for"; parens [ for_init fi; expr e1; expr e2 ] ];
            stmt s ]
  | TryStmt (_,body,catches,EmptyStmt _) ->
      vert (text "try" :: block body :: (map catch catches))
  | TryStmt (_,body,catches,finally) ->
      vert [ text "try"; block body; sep (map catch catches); 
             text "finally"; block finally ]
  | ThrowStmt (_,e) -> sep [text "throw"; expr e; text ";"]
  | ReturnStmt (_,e) ->  sep [ text "return"; nest (expr e); text ";" ]
  | WithStmt (e,s) -> sep [text "with"; paren_exp e; stmt s]
  | VarDeclStmt (_,decls) ->
      sep [ text "var"; 
            vert (commas (map varDecl decls));
            text ";" ]
  | FuncStmt (_,name,args,body) ->
      sep [text "function"; text name; parens (intersperse (text ",") 
                                                 (List.map text args));
                 block body]




let render_expr e : string = to_string expr e

let render_prog (Prog (_, ss)) : string = 
  to_string (fun ss -> vert (map stmt ss)) ss

let render_prefixOp op = to_string prefixOp op

let render_infixOp op = to_string infixOp op

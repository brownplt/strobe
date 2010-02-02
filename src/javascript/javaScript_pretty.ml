open Easy_format
open JavaScript_syntax
open Prelude


let seq_list = { list with 
                 space_after_separator = false; 
                 space_after_opening = false;
                 space_before_closing = false
               }

let text s = Atom (s, { atom_style = Some "atom" })


let parensBy sep xs = List (("(", sep, ")", seq_list), xs)


let brackets xs = List (("[", ",", "]", list), xs)

let bracesBy sep xs = List (("{", sep, "}", list), xs)


let sepBy s xs = 
  List (("",s, "", seq_list), xs)

let sepEndBy s e xs = List (("",s,e, seq_list), xs)

let seq xs = List (("","","",seq_list),xs)

let spaces xs = List ((""," ","",seq_list),xs)

(* concatenate (no spaces) *)
let (@@) x y = List (("","","",seq_list),[x;y])


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


let int n = text (string_of_int n)


let prop (p : prop) = match p with
    PropId x -> text x
  | PropString s -> string s
  | PropNum n -> int n


let rec varDecl (decl : pos varDecl) = match decl with
    VarDeclNoInit (_,x) -> text x
  | VarDecl (_,x,e) -> sepBy " " [text x; text "="; expr e]


and caseClause (clause : pos caseClause) = match clause with
    CaseClause (_,e,s) -> sepBy " " [expr e; text ":"; stmt s]
  | CaseDefault (_,s) -> sepBy " " [text "default:"; stmt s]


and parens e = match e with
    ParenExpr _ -> expr e
  | _ -> List (("(", "", ")", list), [expr e])


and block s = match s with
    BlockStmt _ -> stmt s
  | _ -> List (("{", "", "}", list), [stmt s])


and for_init fi = match fi with
    NoForInit -> text ""
  | VarForInit decls -> spaces [text "var"; sepBy "," (List.map varDecl decls)]
  | ExprForInit e -> expr e


and for_in_init fii = match fii with
    VarForInInit (_,x) -> spaces [text "var"; text x]
  | NoVarForInInit (_,x) -> text x


and lvalue (lv : pos lvalue) = match lv with
    VarLValue (_,x) -> text x
  | DotLValue (_,e,x) -> sepBy "" [expr e; text "."; text x]
  | BracketLValue (_,e1,e2) -> seq [expr e1; brackets [expr e2]]


and catch (clause : pos catch) = match clause with
    CatchClause (_,x,s) -> seq [text "catch"; parensBy "" [text x]; block s]


and expr (e : pos expr) = match e with
    StringExpr (_,s) -> string s
  | RegexpExpr (p,s,g,i) -> string s (* TODO: fixme *)
  | NumExpr (_,f) -> text (string_of_float f)
  | IntExpr (_,n) -> int n
  | BoolExpr (_,true) -> text "true"
  | BoolExpr (_,false) -> text "false"
  | NullExpr _ -> text "null"
  | ArrayExpr (_,es) -> brackets (List.map expr es)
  | ObjectExpr (_,ps) -> 
      let f (p,e) = sepBy "" [prop p; text ":"; expr e]
      in bracesBy "," (List.map f ps)
  | ThisExpr _ -> text "this"
  | VarExpr (_,x) -> text x
  | DotExpr (_,e,x) -> seq [expr e; text "."; text x]
  | BracketExpr (_,e1,e2) -> sepBy "" [expr e1; text "["; expr e2; text "]"]
  | NewExpr (_,constr,args) -> 
      sepBy "" [text "new "; expr constr; parensBy "," (List.map expr args)]
  | PrefixExpr (_,op,e) -> sepBy "" [prefixOp op; expr e]
  | UnaryAssignExpr (_, op, lv) -> 
      if prefix_unaryAssignOp op
      then unaryAssignOp op @@ lvalue lv
      else lvalue lv @@ unaryAssignOp op
  | InfixExpr (_,op,e1,e2) ->
      spaces [expr e1; infixOp op; expr e2]
  | IfExpr (_,e1,e2,e3) ->
      sepBy " " [expr e1; text "?"; expr e2; text ":"; expr e3]
  | AssignExpr (_,op,lv,e) -> spaces [lvalue lv; assignOp op; expr e]
  | ParenExpr (_,e) -> parensBy "" [expr e]
  | ListExpr (_,e1,e2) -> sepBy "," [expr e1; expr e2]
  | CallExpr (_,func,args) ->
      (expr func) @@ (parensBy "," (List.map expr args))
  | FuncExpr (_,args,body) ->
      sepBy " " [text "function"; parensBy "," (List.map text args); stmt body]
  | NamedFuncExpr (_,name,args,body) ->
      sepBy " " [text "function"; text name; parensBy "," (List.map text args);
                 stmt body]
  | UndefinedExpr _ -> text ""

and stmt (s : pos stmt) = match s with
   BlockStmt (_,ss) -> bracesBy "" (List.map stmt ss)
  | EmptyStmt _ -> text ";"
  | ExprStmt e -> sepEndBy "" ";" [expr e]
  | IfStmt (_,e,s1,s2) ->
      sepBy " " [text "if"; parens e; stmt s1; text "else"; stmt s2]
  | IfSingleStmt (_,e,s1) -> sepBy " " [text "if"; parens e; stmt s1]
  | SwitchStmt (_,e,clauses) ->
      sepBy " " [text "switch"; parens e;
                 bracesBy "" (List.map caseClause clauses)]
  | WhileStmt (_,e,s) -> sepBy " " [text "while"; parens e; stmt s]
  | DoWhileStmt (_,s,e) -> 
      sepBy " " [text "do"; stmt s; text "while"; parens e]
  | BreakStmt _ -> text "break;"
  | BreakToStmt (_,x) -> text ("break " ^ x ^ ";") 
  | ContinueStmt _ -> text "continue;"
  | ContinueToStmt  (_,x) -> text ("continue " ^ x ^ ";")
  | LabelledStmt (_,x,s) -> sepBy " " [text (x ^ ":"); stmt s]
  | ForInStmt (_,fii,e,s) ->
      spaces [text "for"; parensBy " " [for_in_init fii; text "in "; expr e];
              block s]
  | ForStmt (_,fi,e1,e2,s) ->
     spaces [text "for"; parensBy "; " [for_init fi; expr e1; expr e2];
             stmt s]
  | TryStmt (_,body,catches,EmptyStmt _) ->
      seq [text "try"; block body; seq (List.map catch catches)]
  | TryStmt (_,body,catches,finally) ->
      seq [text "try"; block body; seq (List.map catch catches); 
           text "finally"; block finally]
  | ThrowStmt (_,e) -> sepBy " " [text "throw"; expr e; text ";"]
  | ReturnStmt (_,e) -> 
      sepEndBy " " ";" [text "return"; expr e]
  | WithStmt (e,s) -> sepBy " " [text "with"; parens e; stmt s]
  | VarDeclStmt (_,decls) ->
      sepEndBy " " ";" [text "var"; sepBy "," (List.map varDecl decls)] 
  | FuncStmt (_,name,args,body) ->
      sepBy " " [text "function"; text name; parensBy "," (List.map text args);
                 block body]




let render_expr (e : pos expr) : string = 
  Easy_format.Pretty.to_string (expr e)

let render_stmts (ss : pos stmt list) : string =
  Easy_format.Pretty.to_string (sepBy "\n" (List.map stmt ss))

let render_prefixOp op =
  Easy_format.Pretty.to_string (prefixOp op)

let render_infixOp op =
  Easy_format.Pretty.to_string (infixOp op)

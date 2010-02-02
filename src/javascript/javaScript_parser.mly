%{
(** A JavaScript parser that does not do semicolon insertion. *)
open Prelude
open JavaScript_syntax

exception Expected_lvalue

exception Parse_failure of string

let rec expr_to_lvalue (e : 'a expr) : ('a lvalue) =  match e with
    VarExpr (p,x) -> VarLValue (p,x)
  | DotExpr (p,e,x) -> DotLValue (p,e,x)
  | BracketExpr (p,e1,e2) -> BracketLValue (p,e1,e2)
  | ParenExpr (_, e) -> expr_to_lvalue e
  | _ -> raise Expected_lvalue

%}

%token <string> ContinueId
%token <string> BreakId
%token <Prelude.pos * string> Id
%token <Prelude.pos * string> String
%token <Prelude.pos * string * bool * bool> Regexp
%token <Prelude.pos * int> Int
%token <Prelude.pos * float> Float

%token If Then Else True False New Instanceof This Null Function Typeof Void
 Delete Switch Default Case While Do Break Var In For Try Catch Finally Throw
 Return With Continue Instanceof

%token LBrace RBrace LParen RParen AssignBOr AssignBXor AssignBAnd AssignLShift
 AssignRShift AssignSpRShift AssignAdd AssignSub AssignMul AssignDiv AssignMod
 Assign Semi Comma Ques Colon LOr LAnd BOr BXor BAnd StrictEq AbstractEq
 StrictNEq AbstractNEq LShift RShift SpRShift LEq LT GEq GT PlusPlus MinusMinus
 Plus Minus Times Div Mod Exclamation Tilde Period LBrack RBrack

%token EOF
%token UPlus UMinus
%token ShiftElse ReduceElse


%left LOr
%left LAnd
%left BOr
%left BXor
%left BAnd
%left StrictEq StrictNEq AbstractEq AbstractNEq
%left LT LEq GT GEq In Instanceof
%left LShift RShift SpRShift
%left Plus Minus
%left Times Div Mod
%left ReduceElse
%left ShiftElse

%start program
%start expression

%type <Prelude.pos JavaScript_syntax.stmt list> program
%type <Prelude.pos JavaScript_syntax.expr> expression

%%

exprs
  : { [] }
  | assign_expr { [$1] }
  | assign_expr Comma exprs { $1::$3 }

stmts
  : { [] }
  | stmt stmts { $1 :: $2 }

cases
  : { [] }
  | case cases { $1 :: $2 }

catches
  : { [] }
  | catch catches { $1 :: $2 }


ids
  : { [] }
  | Id { let _,x = $1 in [x] }
  | Id Comma ids { let _,x = $1 in x :: $3 }

prop
  : Id { let _,x = $1 in PropId x }
  | String { let _,s = $1 in PropString s }

fields
  : { [] }
  | prop Colon expr Comma fields { ($1,$3)::$5 }

varDecls
  : varDecl { [$1] }
  | varDecl Comma varDecls { $1::$3 } 

varDecls_noin
  : varDecl_noin { [$1] }
  | varDecl_noin Comma varDecls_noin { $1::$3 } 


/*******************************************************************************
 * Expressions
 */

element_list
  : 
      { [] }
  | Comma 
      { [UndefinedExpr ((symbol_start_pos (), symbol_end_pos ()))] }
  | assign_expr Comma element_list 
      { $1::$3 }

primary_expr
  : True 
      { BoolExpr ((symbol_start_pos (), symbol_end_pos ()), true) }
  | False 
      { BoolExpr ((symbol_start_pos (), symbol_end_pos ()), false) }
  | This 
      { ThisExpr ((symbol_start_pos (), symbol_end_pos ())) }
  | Null
      { NullExpr ((symbol_start_pos (), symbol_end_pos ())) }  
  | Id 
      { let loc,x = $1 in VarExpr (loc,x) }
  | LBrack element_list RBrack
      { ArrayExpr ((symbol_start_pos (), symbol_end_pos ()),$2) }
  | LBrace fields RBrace 
      { ObjectExpr ((symbol_start_pos (), symbol_end_pos ()),$2) }
  | String
      { let loc, s = $1 in StringExpr (loc, s) }
  | Regexp
      { let loc,re,g,ci = $1 in RegexpExpr (loc, re, g, ci) }
  | Int
      { let loc,n = $1 in IntExpr (loc,n) }
  | Float
      { let loc,f = $1 in NumExpr (loc,f) }
  | LParen expr RParen
      { ParenExpr ((symbol_start_pos (), symbol_end_pos ()),$2) }

member_expr
  : primary_expr 
      { $1 }
  | Function LParen ids RParen LBrace src_elts RBrace
    { FuncExpr ((symbol_start_pos (), symbol_end_pos ()), $3, BlockStmt ((symbol_start_pos (), symbol_end_pos ()), $6)) }
/* Reduce/reduce conflict with function statements.  Who here knew that
   named function expressions existed?  
  | Function Id LParen ids RParen LBrace src_elts RBrace
    { let _,x = $2 in NamedFuncExpr ($1, x, $4, BlockStmt ($6, $7)) }
*/
  | member_expr Period Id 
      { let _,x = $3 in DotExpr ((symbol_start_pos (), symbol_end_pos ()),$1,x) } 
  | member_expr LBrack expr RBrack
      { BracketExpr ((symbol_start_pos (), symbol_end_pos ()),$1,$3) }
  | New member_expr LParen exprs RParen 
    { NewExpr ((symbol_start_pos (), symbol_end_pos ()),$2,$4) }
  
new_expr
  : member_expr
      { $1 }
  | New new_expr
      { NewExpr ((symbol_start_pos (), symbol_end_pos ()),$2,[]) }


call_expr
  : member_expr LParen exprs RParen
      { CallExpr ((symbol_start_pos (), symbol_end_pos ()),$1,$3) }
  | call_expr LParen exprs RParen
      { CallExpr ((symbol_start_pos (), symbol_end_pos ()),$1,$3) }
  | call_expr LBrack expr RBrack 
      { BracketExpr ((symbol_start_pos (), symbol_end_pos ()),$1,$3) }
  | call_expr Period Id 
      { let _,x = $3 in DotExpr ((symbol_start_pos (), symbol_end_pos ()),$1,x) }

lhs_expr
  : new_expr
      { $1 }
  | call_expr 
      { $1 }

postfix_expr
  : lhs_expr 
      { $1 }
  | lhs_expr PlusPlus
      { UnaryAssignExpr ((symbol_start_pos (), symbol_end_pos ()),PostfixInc,expr_to_lvalue $1) }
  | lhs_expr MinusMinus
      { UnaryAssignExpr ((symbol_start_pos (), symbol_end_pos ()),PostfixDec,expr_to_lvalue $1) }

unary_expr
  : postfix_expr 
      { $1 }
  | PlusPlus unary_expr 
      { UnaryAssignExpr ((symbol_start_pos (), symbol_end_pos ()),PrefixInc,expr_to_lvalue $2) }
  | Exclamation unary_expr 
      { PrefixExpr ((symbol_start_pos (), symbol_end_pos ()),PrefixLNot,$2) } 
  | Tilde unary_expr 
      { PrefixExpr ((symbol_start_pos (), symbol_end_pos ()),PrefixBNot,$2) }
  | Minus unary_expr %prec UMinus
      { PrefixExpr ((symbol_start_pos (), symbol_end_pos ()),PrefixMinus,$2) }
  | Plus unary_expr %prec UPlus 
      { PrefixExpr ((symbol_start_pos (), symbol_end_pos ()),PrefixPlus,$2) }
  | Typeof unary_expr
      { PrefixExpr ((symbol_start_pos (), symbol_end_pos ()),PrefixTypeof,$2) }
  | Void unary_expr
      { PrefixExpr ((symbol_start_pos (), symbol_end_pos ()),PrefixVoid,$2) }
  | Delete unary_expr 
      { PrefixExpr ((symbol_start_pos (), symbol_end_pos ()),PrefixDelete,$2) }

/* Combines UnaryExpression, MultiplicativeExpression, AdditiveExpression, and
 * ShiftExpression by using precedence and associativity rules.
 */
op_expr
  : unary_expr { $1 }
  | op_expr Times op_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpMul,$1,$3) }
  | op_expr Div op_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpDiv,$1,$3) }
  | op_expr Mod op_expr 
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpMod,$1,$3) }
  | op_expr Plus op_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpAdd,$1,$3) }
  | op_expr Minus op_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpSub,$1,$3) }
  | op_expr LShift op_expr 
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpLShift,$1,$3) }
  | op_expr RShift op_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpZfRShift,$1,$3) }
  | op_expr SpRShift op_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpSpRShift,$1,$3) }

in_expr
  : op_expr 
      { $1 }
  | in_expr LT in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpLT,$1,$3) }
  | in_expr GT in_expr 
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpGT,$1,$3) }
  | in_expr LEq in_expr 
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpLEq,$1,$3) }
  | in_expr GEq in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpGEq,$1,$3) }
  | in_expr Instanceof in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpInstanceof,$1,$3) }
  | in_expr In in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpIn,$1,$3) }
  | in_expr StrictEq in_expr 
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpStrictEq,$1,$3) }
  | in_expr StrictNEq in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpStrictNEq,$1,$3) }
  | in_expr AbstractEq in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpEq,$1,$3) }
  | in_expr AbstractNEq in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpNEq,$1,$3) }
  | in_expr BAnd in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpBAnd,$1,$3) }
  | in_expr BXor in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpBXor,$1,$3) }
  | in_expr BOr in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpBOr,$1,$3) }
  | in_expr LAnd in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpLAnd,$1,$3) }
  | in_expr LOr in_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpLOr,$1,$3) }

cond_expr
  : in_expr
      { $1 }
  | in_expr Ques assign_expr Colon assign_expr 
      { IfExpr ((symbol_start_pos (), symbol_end_pos ()),$1,$3,$5) }


assign_expr
  : cond_expr
      { $1 }
  | lhs_expr Assign assign_expr 
    { AssignExpr ((symbol_start_pos (), symbol_end_pos ()),OpAssign,expr_to_lvalue $1,$3) }
  | lhs_expr AssignBOr assign_expr 
    { AssignExpr ((symbol_start_pos (), symbol_end_pos ()),OpAssignBOr,expr_to_lvalue $1,$3) }

expr 
  : assign_expr 
      { $1 }
  | expr Comma assign_expr
      { ListExpr ((symbol_start_pos (), symbol_end_pos ()),$1,$3) }

noin_expr
  : op_expr
      { $1 }
  | noin_expr LT noin_expr 
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpLT,$1,$3) }
  | noin_expr GT noin_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpGT,$1,$3) }
  | noin_expr LEq noin_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpLEq,$1,$3) }
  | noin_expr GEq noin_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpGEq,$1,$3) }
  | noin_expr Instanceof noin_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpInstanceof,$1,$3) }
  | noin_expr StrictEq noin_expr 
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpStrictEq,$1,$3) }
  | noin_expr StrictNEq noin_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpStrictNEq,$1,$3) }
  | noin_expr AbstractEq noin_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpEq,$1,$3) }
  | noin_expr AbstractNEq noin_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpNEq,$1,$3) }
  | noin_expr BAnd noin_expr 
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpBAnd,$1,$3) }
  | noin_expr BXor noin_expr 
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpBXor,$1,$3) }
  | noin_expr BOr noin_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpBOr,$1,$3) }
  | noin_expr LAnd noin_expr
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpLAnd,$1,$3) }
  | noin_expr LOr noin_expr 
      { InfixExpr ((symbol_start_pos (), symbol_end_pos ()),OpLOr,$1,$3) }

cond_noin_expr
  : noin_expr { $1 }
  | noin_expr Ques assign_noin_expr Colon assign_noin_expr 
    { IfExpr ((symbol_start_pos (), symbol_end_pos ()),$1,$3,$5) }


assign_noin_expr
  : cond_noin_expr { $1 }
  | lhs_expr Assign assign_noin_expr 
    { AssignExpr ((symbol_start_pos (), symbol_end_pos ()),OpAssign,expr_to_lvalue $1,$3) }
  | lhs_expr AssignBOr assign_noin_expr 
    { AssignExpr ((symbol_start_pos (), symbol_end_pos ()),OpAssignBOr,expr_to_lvalue $1,$3) }

expr_noin
  : assign_noin_expr { $1 }
  | noin_expr Comma assign_noin_expr 
      { ListExpr ((symbol_start_pos (), symbol_end_pos ()),$1,$3) }



/*******************************************************************************
 * Statements 
 */

varDecl
  : Id
      { let loc,x = $1 in VarDeclNoInit (loc,x) }
  | Id Assign assign_expr
      { let _,x = $1 in VarDecl ((symbol_start_pos (), symbol_end_pos ()),x,$3) }

varDecl_noin
  : Id
      { let loc,x = $1 in VarDeclNoInit (loc,x) }
  | Id Assign assign_noin_expr 
      { let _,x = $1 in VarDecl ((symbol_start_pos (), symbol_end_pos ()),x,$3) }


case
  : Case expr Colon stmts 
  { CaseClause ((symbol_start_pos (), symbol_end_pos ()),$2,BlockStmt ((symbol_start_pos (), symbol_end_pos ()),$4)) }
  | Default Colon stmts
  { CaseDefault ((symbol_start_pos (), symbol_end_pos ()),BlockStmt ((symbol_start_pos (), symbol_end_pos ()),$3)) }


forInInit
  : Id { let loc,x = $1 in NoVarForInInit (loc,x) }
  | Var Id 
  { let _,x = $2 in VarForInInit ((symbol_start_pos (), symbol_end_pos ()),x) }

forInit
  : { NoForInit }
  | Var varDecls_noin { VarForInit $2 }
  | expr_noin { ExprForInit $1 }

catch
  : Catch LParen Id RParen block
    { let _,x = $3 in CatchClause ((symbol_start_pos (), symbol_end_pos ()),x,$5) }


block : LBrace stmts RBrace
      { BlockStmt ((symbol_start_pos (), symbol_end_pos ()),$2) }

paren_expr : LParen expr RParen
      { ParenExpr ((symbol_start_pos (), symbol_end_pos ()),$2) }


stmt 
  : LBrace stmt stmts RBrace
      { BlockStmt ((symbol_start_pos (), symbol_end_pos ()),$2::$3) }
  | Semi 
      { EmptyStmt ((symbol_start_pos (), symbol_end_pos ())) }
  | expr Semi 
      { ExprStmt $1 }
  | Continue Semi 
      { ContinueStmt ((symbol_start_pos (), symbol_end_pos ())) }
  | ContinueId Semi 
      { ContinueToStmt ((symbol_start_pos (), symbol_end_pos ()),$1) }
  | If LParen expr RParen stmt Else stmt
    { IfStmt ((symbol_start_pos (), symbol_end_pos ()), $3, $5, $7) }
  | If LParen expr  RParen stmt
    { IfSingleStmt ((symbol_start_pos (), symbol_end_pos ()), $3, $5) }
  | Switch paren_expr LBrace cases RBrace 
      { SwitchStmt ((symbol_start_pos (), symbol_end_pos ()),$2,$4) }
  | While paren_expr block
      { WhileStmt ((symbol_start_pos (), symbol_end_pos ()),$2,$3) }
  | Do block While paren_expr Semi
      { DoWhileStmt ((symbol_start_pos (), symbol_end_pos ()),$2,$4) }
  | Break  Semi
      { BreakStmt ((symbol_start_pos (), symbol_end_pos ())) }
  | BreakId Semi
      { BreakToStmt ((symbol_start_pos (), symbol_end_pos ()),$1) }
  | Id Colon stmt
      { let _,x = $1 in LabelledStmt ((symbol_start_pos (), symbol_end_pos ()),x,$3) }
  | For LParen forInInit In expr RParen block
    { ForInStmt ((symbol_start_pos (), symbol_end_pos ()),$3,$5,$7) }
  | For LParen forInit Semi expr Semi expr RParen block
    { ForStmt ((symbol_start_pos (), symbol_end_pos ()),$3,$5,$7,$9) }
  | Try block catches
    { TryStmt ((symbol_start_pos (), symbol_end_pos ()),$2,$3,EmptyStmt ((symbol_start_pos (), symbol_end_pos ()))) }
  | Try block catches Finally block { TryStmt ((symbol_start_pos (), symbol_end_pos ()),$2,$3,$5) }
  | Throw expr Semi 
      { ThrowStmt ((symbol_start_pos (), symbol_end_pos ()),$2) }
  | Return Semi 
      { ReturnStmt ((symbol_start_pos (), symbol_end_pos ()),UndefinedExpr ((symbol_start_pos (), symbol_end_pos ()))) }
  | Return expr Semi 
      { ReturnStmt ((symbol_start_pos (), symbol_end_pos ()),$2) } 
  | Var varDecls Semi
      { VarDeclStmt ((symbol_start_pos (), symbol_end_pos ()),$2) }

src_elt_block
  : LBrace src_elts RBrace
      { BlockStmt ((symbol_start_pos (), symbol_end_pos ()),$2) }
  
src_elts
  : { [] }
  | src_elt src_elts { $1::$2 }

src_elt
  : stmt { $1 }
  | Function Id LParen ids RParen src_elt_block
    { let _,x = $2 in FuncStmt ((symbol_start_pos (), symbol_end_pos ()),x,$4,$6) } 

program : src_elts EOF { $1 }

expression : expr EOF { $1 }

%%

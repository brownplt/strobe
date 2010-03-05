%{
open Prelude
open Lambdajs_syntax
open Exprjs_syntax
%}

%token <int> INT
%token <float> NUM
%token <string> STRING
%token <bool> BOOL
%token <Prelude.id> ID
%token UNDEFINED NULL FUNC RARROW LET DELETE LBRACE RBRACE LPAREN RPAREN LBRACK
  RBRACK EQUALS COMMA DEREF REF COLON IN COLONEQ PRIM IF THEN ELSE SEMI
  LABEL BREAK TRY CATCH FINALLY THROW


%token EOF
%nonassoc GT_SEMI
%nonassoc SEMI
%nonassoc LT_SEMI

/* http://stackoverflow.com/questions/1737460/
   how-to-find-shift-reduce-conflict-in-this-yacc-file */

%type <Lambdajs_syntax.exp> prog
%start prog


%%

const :
 | NUM { CNum $1 }
 | INT { CInt $1 }
 | STRING { CString $1 }
 | UNDEFINED { CUndefined }
 | NULL { CNull }
 | BOOL { CBool $1 }

prop :
 | ID COLON exp { (($startpos, $endpos), $1, $3) }

props :
 | { [] }
 | prop { [$1] }
 | prop COMMA props { $1 :: $3 }

exps :
 | { [] }
 | exp { [$1] }
 | exp COMMA exps { $1 :: $3 }

simpl_exp :
 | const { EConst (($startpos, $endpos), $1) }
 | ID { EId (($startpos, $endpos), $1) }
 | LBRACE props RBRACE 
   { EObject (($startpos, $endpos), $2) }
 | LPAREN ref_exp RPAREN
     { $2 }

exp :
 | simpl_exp { $1 }
 | exp LPAREN exps RPAREN 
   { EApp (($startpos, $endpos), $1, $3) }
 | exp LBRACK exp RBRACK
   { EOp2 (($startpos, $endpos), GetField, $1, $3) }
 | exp LBRACK exp EQUALS exp RBRACK
   { EUpdateField (($startpos, $endpos), $1, $3, $5) }
 | exp LBRACK DELETE exp RBRACK
   { EOp2 (($startpos, $endpos), DeleteField, $1, $4) }
 | PRIM LPAREN STRING COMMA exp COMMA exp RPAREN
   { EOp2 (($startpos, $endpos), Prim2 $3, $5, $7) }

ref_exp :
 | exp { $1 }
 | REF ref_exp 
   { EOp1 (($startpos, $endpos), Ref, $2) }
 | DEREF ref_exp 
   { EOp1 (($startpos, $endpos), Deref, $2) }
 | exp COLONEQ ref_exp
   { EOp2 (($startpos, $endpos), SetRef, $1, $3) }

ctrl_exp :
 | ref_exp { $1 }
 | LET ID EQUALS ref_exp IN ctrl_exp
   { ELet (($startpos, $endpos), $2, $4, $6) }
   %prec LT_SEMI 
   /* let x = e1 in e2; let y = e3 in ... ==>
      let x = e1 in e2; (let y = e3 in ...) */
 | IF ctrl_exp THEN ctrl_exp ELSE ctrl_exp
     { EIf (($startpos, $endpos), $2, $4, $6) }
 | ctrl_exp SEMI ctrl_exp 
     { ESeq (($startpos, $endpos), $1, $3) }
 | LABEL ID COLON ctrl_exp
     { ELabel (($startpos, $endpos), $2, $4) } 
 | FUNC LPAREN ID* RPAREN LBRACE RARROW ctrl_exp
   { ELambda (($startpos, $endpos), $3, $7) }
 | BREAK ID ctrl_exp
   { EBreak (($startpos, $endpos), $2, $3) }
 | THROW ctrl_exp
   { EThrow (($startpos, $endpos), $2) }
   %prec GT_SEMI /* thow e1; e2 ==> (throw e1); e2 */
 | TRY ctrl_exp CATCH ctrl_exp
   { ETryCatch (($startpos, $endpos), $2, $4) }
 | TRY ctrl_exp FINALLY ctrl_exp
   { ETryFinally (($startpos, $endpos), $2, $4) }



prog :
 | ctrl_exp EOF { $1 }
%%

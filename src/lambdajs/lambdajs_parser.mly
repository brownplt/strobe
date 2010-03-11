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
%token UNDEFINED NULL FUNC LET DELETE LBRACE RBRACE LPAREN RPAREN LBRACK
  RBRACK EQUALS COMMA DEREF REF COLON COLONEQ PRIM IF ELSE SEMI
  LABEL BREAK TRY CATCH FINALLY THROW LLBRACK RRBRACK EQEQEQUALS TYPEOF
  AMPAMP PIPEPIPE RETURN BANGEQEQUALS FUNCTION


%token EOF
%right COLONEQ
%left PIPEPIPE
%left AMPAMP
%left EQEQEQUALS BANGEQEQUALS
%nonassoc TYPEOF DEREF REF

/* http://stackoverflow.com/questions/1737460/
   how-to-find-shift-reduce-conflict-in-this-yacc-file */

%type <Lambdajs_syntax.exp> prog
%type <(Prelude.id * Lambdajs_syntax.exp) list> env

%start prog
%start env


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
 | seq_exp { [$1] }
 | seq_exp COMMA exps { $1 :: $3 }

ids :
 | { [] }
 | ID { [$1] }
 | ID COMMA ids { $1 :: $3 }


atom :
 | const { EConst (($startpos, $endpos), $1) }
 | ID { EId (($startpos, $endpos), $1) }
 | LBRACE props RBRACE 
   { EObject (($startpos, $endpos), $2) }
 | LBRACE seq_exp RBRACE
   { $2 }
 | LPAREN seq_exp RPAREN { $2 }
 | FUNC LPAREN ids RPAREN LBRACE RETURN seq_exp RBRACE
   { ELambda (($startpos, $endpos), $3, $7) }
 | FUNCTION LPAREN ids RPAREN LBRACE RETURN seq_exp RBRACE
   { let p = ($startpos, $endpos) in
     let fields = 
       [ (p, "__code__", ELambda (p, $3, $7));
         (p, "arguments", EConst (p, CNull));
         (p, "prototype", EOp1 (p, Ref,
                                EObject 
                                  (p, [(p, "__proto__",
                                        EId (p, "[[Object_prototype]]"))])));
         (p, "__proto__", EId (p, "[[Function_prototype]]"));
         (p, "length", EConst (p, CInt (List.length $3)));
         (p, "__string__", EConst (p, CString "[ builtin function ]")) ] in
       EOp1 (p, Ref, EObject (p, fields)) }
 | atom LPAREN exps RPAREN 
   { EApp (($startpos, $endpos), $1, $3) }
 | atom LBRACK seq_exp RBRACK
   { EOp2 (($startpos, $endpos), GetField, $1, $3) }
 | atom LBRACK seq_exp EQUALS seq_exp RBRACK
   { EUpdateField (($startpos, $endpos), $1, $3, $5) }
 | atom LBRACK DELETE seq_exp RBRACK
   { EOp2 (($startpos, $endpos), DeleteField, $1, $4) }


exp :
 | atom { $1 }
 | PRIM LPAREN STRING COMMA seq_exp COMMA seq_exp RPAREN
   { EOp2 (($startpos, $endpos), Prim2 $3, $5, $7) }
 | PRIM LPAREN STRING COMMA seq_exp RPAREN
   { EOp1 (($startpos, $endpos), Prim1 $3, $5) }
 | REF exp 
   { EOp1 (($startpos, $endpos), Ref, $2) }
 | DEREF exp 
   { EOp1 (($startpos, $endpos), Deref, $2) }
 | exp COLONEQ exp
   { EOp2 (($startpos, $endpos), SetRef, $1, $3) }
 | exp EQEQEQUALS exp
     { EOp2 (($startpos, $endpos), Prim2 "stx=", $1, $3) }
 | exp BANGEQEQUALS exp
     { let p = ($startpos, $endpos) in
         EIf (p, EOp2 (p, Prim2 "stx=", $1, $3),
              EConst (p, CBool false),
              EConst (p, CBool true)) }
 | TYPEOF exp
     { EOp1 (($startpos, $endpos), Prim1 "typeof", $2) }
 | exp AMPAMP exp
     { EIf (($startpos, $endpos), $1, 
            $3,
            EConst (($startpos, $endpos), CBool false)) }
 | exp PIPEPIPE exp
     { let p = ($startpos, $endpos) in
         ELet (p, "%or", $1,
               EIf (p, EId (p, "%or"), EId (p, "%or"), $3)) }


cexp :
 | exp { $1 }
 | IF LPAREN seq_exp RPAREN seq_exp ELSE seq_exp
     { EIf (($startpos, $endpos), $3, $5, $7) }

 | LABEL ID COLON cexp
     { ELabel (($startpos, $endpos), $2, $4) } 
 | BREAK ID cexp
   { EBreak (($startpos, $endpos), $2, $3) }
 | THROW cexp
   { EThrow (($startpos, $endpos), $2) }
 | TRY LBRACE seq_exp RBRACE CATCH LBRACE seq_exp RBRACE
   { ETryCatch (($startpos, $endpos), $3, $7) }
 | TRY LBRACE seq_exp RBRACE FINALLY LBRACE seq_exp RBRACE
   { ETryFinally (($startpos, $endpos), $3, $7) }

seq_exp :
 | cexp { $1 }
 | LET LPAREN ID EQUALS seq_exp RPAREN seq_exp
   { ELet (($startpos, $endpos), $3, $5, $7) }
 | cexp SEMI seq_exp
   { ESeq (($startpos, $endpos), $1, $3) }
      

env_bind :
 | LLBRACK ID RRBRACK EQUALS seq_exp { ($2, $5) }

env :
 | EOF { [] }
 | env_bind env { $1 :: $2 }

prog :
 | seq_exp EOF { $1 }
%%

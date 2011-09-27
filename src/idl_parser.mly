%{

open Idl_syntax
%}

%token <Id.t> ID 
%token <int> INT
%token MODULE INTERFACE LBRACE RBRACE SEMI SHORT LONG LONGLONG BOOLEAN
       BYTE OCTET FLOAT DOUBLE DOMSTRING OBJECT DATE ANY QUES LRBRACK
       UNSIGNED READONLY VOID COMMA IN OPTIONAL LPAREN RPAREN EOF
       ATTRIBUTE LBRACK RBRACK TYPEDEF EXCEPTION CONST EQUALS RAISES
       COLON

%type <Idl_syntax.definition list> idlFile
%start idlFile

%%

idlFile :
  | definitions EOF  { $1 }

definitions :
  | { [] }
  | definition definitions { $1::$2 }

definition :
  | MODULE ID LBRACE definitions RBRACE SEMI 
    { Module ($startpos, $2, $4) }
  | INTERFACE ID SEMI { ForwardInterface ($startpos, $2) }
  | INTERFACE ID inheritance LBRACE interfaceMembers RBRACE SEMI
    { Interface ($startpos, $2, $3, $5) }
  | TYPEDEF typeWithSuffix ID SEMI { Typedef ($startpos, $2, $3) }
  | EXCEPTION ID LBRACE exceptionMembers RBRACE SEMI
    { Exception ($startpos, $2, $4) }
  | CONST attributeType ID EQUALS constExpr SEMI
    { Const ($startpos, $2, $3) }

constExpr :
  | INT { () }

inheritance :
  | { None }
  | COLON scopedName { Some $2 }

scopedName :
  | ID { RelativeName [$1] }

interfaceMembers :
  | { [] }
  | interfaceMember interfaceMembers { $1::$2 }

exceptionMembers :
  | { [] }
  | exceptionMember exceptionMembers { $1::$2 }

exceptionMember :
  | attributeType ID SEMI
    { Attribute ($startpos, NoReadOnly, $1, $2) }

interfaceMember :
  | readOnly ATTRIBUTE attributeType ID SEMI
    { Attribute ($startpos, $1, $3, $4) }
  | returnType ID LPAREN argumentList RPAREN raisesClause SEMI
    { Operation ($startpos, $1, $2, $4) }
  | CONST attributeType ID EQUALS constExpr SEMI
    { ConstMember ($startpos, $2, $3) }

ids:
  | { [] }
  | ID { [$1] }
  | ID COMMA ids { $1::$3 }

raisesClause :
  | { () }
  | RAISES LPAREN ids RPAREN { () }

primitiveOrStringType :
  | unsigned SHORT { Short $1 }
  | unsigned LONG { Long $1 }
  | unsigned LONGLONG { LongLong $1 }
  | BOOLEAN { Boolean }
  | BYTE { Byte }
  | OCTET { Octet }
  | FLOAT { Float }
  | DOUBLE { Double }
  | DOMSTRING { DOMString }

attributeType :
  | typeWithSuffix { $1 }

typeWithSuffix :
  | primitiveOrStringType { $1 }
  | typeWithSuffix QUES { Ques $1 }
  | typeWithSuffix LRBRACK { Array $1 }
  | ANY { Any }
  | OBJECT { Object }
  | scopedName { Name $1 }

unsigned :
  | { NoUnsigned }
  | UNSIGNED { Unsigned }


readOnly :
  | READONLY { ReadOnly }
  | { NoReadOnly }


returnType :
  | attributeType { $1 }
  | VOID { Void }

argumentList :
  | { [] }
  | argument  { [$1] }
  | argument COMMA argumentList { $1 :: $3 }

argument :
  | in_ optional attributeType ID { $3 }

in_ :
  | { () }
  | IN { () }

optional :
  |  { () }
  | OPTIONAL { () }

%%

%{

open Idl_syntax
%}

%token <Id.t> ID 
%token <int> INT
%token MODULE INTERFACE LBRACE RBRACE SEMI SHORT LONG LONGLONG BOOLEAN
       BYTE OCTET FLOAT DOUBLE DOMSTRING OBJECT DATE ANY QUES LRBRACK
       UNSIGNED READONLY VOID COMMA IN OPTIONAL LPAREN RPAREN EOF
       ATTRIBUTE LBRACK RBRACK TYPEDEF EXCEPTION CONST EQUALS RAISES
       COLON DOTDOTDOT LEGACYCALLER GETTER SETTER CREATOR DELETER
       NOINTERFACEOBJECT OVERRIDEBUILTINS PUTFORWARDS IMPLEMENTS
       STRINGIFIER NAMEDCONSTRUCTOR CONSTRUCTOR REPLACEABLENAMEDPROPERTIES
       UNFORGEABLE REPLACEABLE DICTIONARY CALLBACK TREATNULLAS FUNCTIONONLY
       ALLOWANY PARTIAL SEQUENCE LANGLE RANGLE CLAMP

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
  | INTERFACE name=ID SEMI { ForwardInterface ($startpos, name) }
  | LBRACK metas=meta_comma RBRACK INTERFACE name=ID super=inheritance 
    LBRACE mems=interfaceMembers RBRACE SEMI
    { Interface ($startpos, name, super, mems, metas) }
  | PARTIAL INTERFACE x=ID LBRACE mems=interfaceMembers RBRACE SEMI
    { PartialInterface ($startpos, x, mems) }
  | INTERFACE name=ID super=inheritance 
    LBRACE mems=interfaceMembers RBRACE SEMI
    { Interface ($startpos, name, super, mems, []) }
  | TYPEDEF typeWithSuffix ID SEMI { Typedef ($startpos, $2, $3) }
  | EXCEPTION ID LBRACE exceptionMembers RBRACE SEMI
    { Exception ($startpos, $2, $4) }
  | CONST attributeType ID EQUALS constExpr SEMI
    { Const ($startpos, $2, $3) }
  | name=ID IMPLEMENTS super=ID  SEMI
    { Implements ($startpos, name, super) }
  | DICTIONARY name=ID inheritance LBRACE 
      members=list(dictionary_member) 
    RBRACE SEMI
    { Dictionary ($startpos, name, members) }

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

dictionary_member :
  | t=attributeType x=ID SEMI { Attribute ($startpos, NoReadOnly, t, x) }

exceptionMember :
  | typ=attributeType name=ID SEMI
    { Attribute ($startpos, NoReadOnly, typ, name) }
  | CONST typ=attributeType name=ID EQUALS constExpr SEMI 
    { ConstMember ($startpos, typ, name) }

interfaceMember :
  | metas STRINGIFIER? readOnly=readOnly ATTRIBUTE typ=attributeType name=ID 
      SEMI
    { Attribute ($startpos, readOnly, typ, name) }
  | legacy ret=returnType name=ID LPAREN args=argumentList RPAREN 
    raisesClause SEMI
    { Operation ($startpos, ret, name, args) }
  | legacy SETTER ret=returnType name=ID LPAREN args=argumentList RPAREN 
    raisesClause SEMI
    { Operation ($startpos, ret, name, args) }
  | legacy GETTER ret=returnType name=ID LPAREN args=argumentList RPAREN 
    raisesClause SEMI
    { Operation ($startpos, ret, name, args) }
  | legacy CREATOR ret=returnType LPAREN args=argumentList RPAREN SEMI
    { Creator ($startpos, ret, args) }
  | legacy GETTER CREATOR ret=returnType LPAREN args=argumentList RPAREN SEMI
    { Creator ($startpos, ret, args) }
  | legacy SETTER CREATOR ret=returnType LPAREN args=argumentList RPAREN SEMI
    { Creator ($startpos, ret, args) }
  | CONST attributeType ID EQUALS constExpr SEMI
    { ConstMember ($startpos, $2, $3) }
  | legacy GETTER ret=returnType LPAREN args=argumentList RPAREN SEMI
    { Getter ($startpos, ret, args) }
  | legacy SETTER ret=returnType LPAREN args=argumentList RPAREN SEMI
    { Setter ($startpos, ret, args) }
  | legacy DELETER ret=returnType LPAREN args=argumentList RPAREN SEMI
    { Deletor ($startpos, ret, args) }

legacy :
  | { () }
  | LEGACYCALLER { () }

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
  | unsigned BYTE { Byte }
  | OCTET { Octet }
  | FLOAT { Float }
  | DOUBLE { Double }
  | DOMSTRING { DOMString }

attributeType :
  | typeWithSuffix { $1 }
  | typeWithSuffix DOTDOTDOT { $1 }

typeWithSuffix :
  | primitiveOrStringType { $1 }
  | typeWithSuffix QUES { Ques $1 }
  | typeWithSuffix LRBRACK { Array $1 }
  | ANY { Any }
  | OBJECT { Object }
  | DATE { Date }
  | scopedName { Name $1 }
  | SEQUENCE LANGLE t=typeWithSuffix RANGLE { Sequence t }

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
  | metas in_ optional t=attributeType ID { t }

in_ :
  | { () }
  | IN { () } 

optional :
  |  { () }
  | OPTIONAL { () }

meta :
  | NOINTERFACEOBJECT { NoInterfaceObject }
  | OVERRIDEBUILTINS { OverrideBuiltins }
  | PUTFORWARDS EQUALS ID { PutForwards $3 }
  | NAMEDCONSTRUCTOR EQUALS constr=ID 
      args=delimited(LPAREN, separated_list(COMMA, argument),
                     RPAREN) 
    { NamedConstructor(constr, args) }
  | CONSTRUCTOR { Constructor ([]) }
  | CONSTRUCTOR 
      args=delimited(LPAREN, separated_list(COMMA, argument),
                     RPAREN) 
    { Constructor args }
  | REPLACEABLENAMEDPROPERTIES { ReplaceableNamedProperties } 
  | REPLACEABLE { Replaceable }
  | UNFORGEABLE { Unforgeable }
  | CALLBACK { Callback }
  | CALLBACK EQUALS FUNCTIONONLY { CallbackFunctionOnly }
  | TREATNULLAS EQUALS t=attributeType { TreatNullAs t }
  | ALLOWANY { AllowAny }
  | CLAMP { Clamp }

meta_comma :
  | metas=separated_list(COMMA, meta) { metas }

metas :
  | { () }
  | LBRACK meta_comma RBRACK  { () }

%%

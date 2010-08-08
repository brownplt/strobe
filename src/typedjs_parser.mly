%{

open Prelude
open Typedjs_syntax
open Typedjs_types

%}

%token <string> ID TID STRING
%token ARROW LPAREN RPAREN ANY STAR COLON EOF CONSTRUCTOR INT NUM UNION STR
       UNDEF BOOL LBRACE RBRACE COMMA VAL LBRACK RBRACK DOT OPERATOR
       PROTOTYPE CLASS UPCAST DOWNCAST LANGLE RANGLE FORALL LTCOLON IS
       CHECKED CHEAT HASHPROTO TREC

%right UNION

%start typ_ann
%start env

%type <Typedjs_syntax.annotation> typ_ann
%type <Typedjs_syntax.env_decl list> env

%%

args
  :  { [] }
  | arg_typ { [$1] }
  | arg_typ STAR args { $1 :: $3 }

field
  : ID COLON typ { ($1, TRef $3) }

fields
  : { [] }
  | field { [$1] }
  | field COMMA fields { $1 :: $3 }
  | COMMA { [] }

proto
  : HASHPROTO COLON ID { $3 }

star
  : STAR COLON typ { TRef $3 }

arg_typ
  : ANY { TTop }
  | INT { typ_int }
  | NUM { typ_num }
  | STR { typ_str }
  | BOOL { typ_bool }
  | UNDEF { typ_undef }
  | arg_typ UNION arg_typ { TUnion ($1, $3) }
  | LBRACE fields RBRACE { TObject $2 }
  | LBRACE fields proto COMMA star RBRACE { TObjStar ($2, $3, $5) }
  | LPAREN typ RPAREN { $2 }
  | ID { TConstr ( $1, [] ) }
  | ID LANGLE typs RANGLE { TConstr ($1, map (fun t -> TRef t) $3) }
  | TID { TId $1 }
  | TREC ID DOT LPAREN typ RPAREN { TRec ($2, $5) }

typ 
  : arg_typ { $1 }
  | args ARROW typ { TArrow (TTop, $1, $3) }
  | LBRACK typ RBRACK args ARROW typ { TArrow ($2, $4, $6) }
  | FORALL ID LTCOLON typ DOT typ { TForall ($2, $4, $6) }
  | FORALL ID DOT typ { TForall ($2, TTop, $4) }


typs :
  | { [] }
  | typ { [$1] }
  | typ COMMA typs { $1 :: $3 }

annotation :
  | typ { ATyp $1 }
  | CHEAT typ { ACheat $2 }
  | UPCAST typ { AUpcast $2 }
  | DOWNCAST typ { ADowncast $2 }
  | CONSTRUCTOR typ { AConstructor $2 }
  | FORALL ID LTCOLON typ { ATypAbs ($2, $4) }
  | FORALL ID { ATypAbs ($2, TTop) }
  | LBRACK typ RBRACK { ATypApp $2 }
  | IS typ { AAssertTyp $2 }

typ_ann :
  | annotation EOF { $1 }

any_id :
  | ID { $1 }
  | STR { "Str" }
  | INT { "Int" }
  | UNDEF { "Undef" }
  | BOOL { "Bool" }
  | NUM { "Num" }

checked :
  | CHECKED { true }
  | { false }

env_decl :
  | CLASS checked any_id PROTOTYPE any_id LBRACE fields RBRACE
    { if $2 then Typedjs_dyn_supp.assume_instanceof_contract $3;
      EnvClass
        ( $3 (* name *) , 
          Some $5 (* prototype type *),
          $7 (* local fields *)) }
  | CLASS checked any_id LBRACE fields RBRACE 
      { if $2 then Typedjs_dyn_supp.assume_instanceof_contract $3;
        EnvClass ($3, None (* root *), $5) }
  | VAL ID COLON typ { EnvBind ($2, $4) }
  | ID COLON typ { EnvBind ($1, TRef $3) }
  | OPERATOR STRING COLON typ { EnvBind ($2, $4) }

env_decls
  : { [] }
  | env_decl env_decls { $1 :: $2 }

env
  : env_decls EOF { $1 }



%%

%{

open Prelude
open Typedjs_syntax
open Typedjs_types

%}

%token <string> ID
%token ARROW LPAREN RPAREN ANY STAR COLON EOF CONSTRUCTOR INT NUM UNION STR
       UNDEF BOOL LBRACE RBRACE COMMA COLONCOLON FUNCTION DOM
       PROTOTYPE CLASS UPCAST DOWNCAST

%right UNION

%start typ
%start typ_ann
%start env

%type <Typedjs_syntax.typ> typ

%type <Typedjs_syntax.annotation> typ_ann

%type <Typedjs_syntax.env_decl list> env

%%

args
  :  { [] }
  | arg_typ { [$1] }
  | arg_typ STAR args { $1 :: $3 }

field
  : ID COLON typ { ($1, $3) }

fields
  : { [] }
  | field { [$1] }
  | field COMMA fields { $1 :: $3 }

arg_typ
  : ANY { TTop }
  | INT { typ_int }
  | NUM { typ_num }
  | STR { typ_str }
  | BOOL { typ_bool }
  | UNDEF { typ_undef }
  | DOM { TDom }
  | arg_typ UNION arg_typ { typ_union IdMap.empty $1 $3 }
  | LBRACE fields RBRACE { TRef (typ_permute (TObject $2)) }
  | LPAREN typ RPAREN { $2 }
  | ID { TApp ( $1, [] ) }

typ 
  : arg_typ { $1 }
  | args ARROW typ { TArrow (TTop, $1, $3) }

annotation :
  | COLON typ { ATyp $2 }
  | UPCAST typ { AUpcast $2 }
  | DOWNCAST typ { ADowncast $2 }
  | COLON CONSTRUCTOR typ { AConstructor $3 }
  | COLONCOLON inferred_anns { AInferred $2 }

typ_ann :
  | annotation EOF { $1 }

inferred_anns : 
  | { [] }
  | FUNCTION ID COLON typ inferred_anns { ATyp $4 :: $5 }
  | FUNCTION COLON typ inferred_anns { ATyp $3 :: $4 }
  | CONSTRUCTOR ID COLON typ inferred_anns { ATyp $4 :: $5 }
  | CONSTRUCTOR COLON typ inferred_anns { ATyp $3 :: $4 }
  

any_id :
  | ID { $1 }
  | STR { "String" }

env_decl
  : CLASS any_id PROTOTYPE typ LBRACE fields RBRACE
    { EnvClass
        ( $2 (* name *) , 
          $4 (* prototype type *),
          $6 (* local fields *)) }
  | ID COLON typ
    { EnvBind ($1, $3) }

env_decls
  : { [] }
  | env_decl env_decls { $1 :: $2 }

env
  : env_decls EOF { $1 }



%%

%{

open Prelude
open Typedjs_syntax
open Typedjs_types

%}

%token <string> ID
%token ARROW LPAREN RPAREN ANY STAR COLON EOF CONSTRUCTOR INT NUM UNION STR
       UNDEF BOOL

%right UNION

%start typ
%start typ_ann

%type <Typedjs_syntax.typ> typ

%type <Typedjs_syntax.annotation> typ_ann

%%

args
  :  { [] }
  | arg_typ { [$1] }
  | arg_typ STAR args { $1 :: $3 }

arg_typ
  : ANY { TTop }
  | INT { typ_int }
  | NUM { typ_num }
  | STR { typ_str }
  | BOOL { typ_bool }
  | UNDEF { typ_undef }
  | arg_typ UNION arg_typ { typ_union $1 $3 }
  | LPAREN typ RPAREN { $2 }

typ 
  : arg_typ { $1 }
  | args ARROW typ { TArrow (TTop, $1, $3) }

typ_ann
  : COLON typ { ATyp $2 }
  | COLON CONSTRUCTOR typ { AConstructor $3 }

%%

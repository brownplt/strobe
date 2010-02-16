%{

open Prelude
open Typedjs_syntax
open Typedjs_types

let parse_error s =
  eprintf "%s : %s\n" 
    (string_of_position (symbol_start_pos (), symbol_end_pos ()))
    s

%}

%token <string> ID
%token ARROW LPAREN RPAREN ANY STAR COLON EOF CONSTRUCTOR INT NUM UNION STR
       UNDEF BOOL LBRACE RBRACE COMMA

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
  | arg_typ UNION arg_typ { typ_union $1 $3 }
  | LBRACE fields RBRACE { TObject $2 }
  | LPAREN typ RPAREN { $2 }

typ 
  : arg_typ { $1 }
  | args ARROW typ { TArrow (TTop, $1, $3) }

typ_ann
  : COLON typ { ATyp $2 }
  | COLON CONSTRUCTOR typ { AConstructor $3 }

%%

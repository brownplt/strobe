%{

open Prelude
open Typedjs_syntax

%}

%token <string> ID TID STRING REGEX
%token ARROW LPAREN RPAREN ANY STAR COLON EOF CONSTRUCTOR INT NUM UNION STR
       UNDEF BOOL LBRACE RBRACE COMMA VAL LBRACK RBRACK DOT OPERATOR
       PROTOTYPE CLASS UPCAST DOWNCAST FORALL LTCOLON IS
       CHECKED CHEAT NULL TRUE FALSE REC INTERSECTION SEMI
       

%right UNION INTERSECTION

%start typ_ann
%start env

%type <Typedjs_syntax.annotation> typ_ann
%type <Typedjs_syntax.env_decl list> env

%%

regex :
  | REGEX { RegLang.parse_regex $startpos $1 }
  
args
  :  { [] }
  | arg_typ { [$1] }
  | arg_typ STAR args { $1 :: $3 }

field
  : regex COLON typ { (($1, RegLang.fsm_of_regex $1), $3) }
  | ID COLON typ 
      { let re = RegLang_syntax.String $1 in
        ((re, RegLang.fsm_of_regex re), $3) }

fields
  : { [] }
  | field { [$1] }
  | field COMMA fields { $1 :: $3 }
  | COMMA { [] }

arg_typ
  : ANY { TTop }
  | INT { TPrim Int }
  | NUM { TPrim Num }
  | STR { TPrim Str }
  | BOOL { typ_bool }
  | TRUE { TPrim True }
  | FALSE { TPrim False }
  | UNDEF { TPrim Undef }
  | NULL { TPrim Null }
  | regex { TRegex ($1, RegLang.fsm_of_regex $1) }
  | arg_typ UNION arg_typ { TUnion ($1, $3) }
  | arg_typ INTERSECTION arg_typ { TIntersect ($1, $3) }
  | LBRACE fields RBRACE { mk_object_typ $2 }
  | LPAREN typ RPAREN { $2 }
  | TID { TId $1 }
  | ID { TSyn $1 }

typ 
  : arg_typ { $1 }
  | args ARROW typ { TArrow (TTop::$1, $3) }
  | LBRACK typ RBRACK args ARROW typ { TArrow ($2::$4, $6) }
  | FORALL ID LTCOLON typ DOT typ { TForall ($2, $4, $6) }
  | FORALL ID DOT typ { TForall ($2, TTop, $4) }
  | REC ID DOT typ { TRec ($2, $4) }


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

op_typ :
  | args ARROW typ { TArrow ($1, $3) }
  | LBRACK typ RBRACK args ARROW typ { TArrow ($2::$4, $6) }
  | FORALL ID LTCOLON typ DOT op_typ { TForall ($2, $4, $6) }
  | FORALL ID DOT op_typ { TForall ($2, TTop, $4) }

env_decl :
  | CLASS checked any_id PROTOTYPE any_id arg_typ (* will disappear *)
    { if $2 then Typedjs_dyn_supp.assume_instanceof_contract $3;
      EnvClass
        ( $3 (* name *) , 
          Some $5 (* prototype type *),
          $6 (* type *)) }
  | CLASS checked any_id arg_typ
      { if $2 then Typedjs_dyn_supp.assume_instanceof_contract $3;
        EnvClass ($3, None (* root *), $4) }
  | VAL ID COLON typ { EnvBind ($2, $4) }
  | ID COLON typ { EnvBind ($1, TRef $3) }
  | OPERATOR STRING COLON op_typ { EnvBind ($2, $4) }

env_decls
  : { [] }
  | env_decl env_decls { $1 :: $2 }

env
  : env_decls EOF { $1 }



%%

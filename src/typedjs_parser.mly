%{

open Prelude
open Typedjs_syntax
module W = Typedjs_syntax.WritTyp

%}

%token <string> ID TID STRING REGEX
%token ARROW LPAREN RPAREN ANY STAR COLON EOF CONSTRUCTOR INT NUM UNION STR
       UNDEF BOOL LBRACE RBRACE COMMA VAL LBRACK RBRACK DOT OPERATOR
       UPCAST DOWNCAST FORALL LTCOLON IS LANGLE RANGLE
       CHEAT NULL TRUE FALSE REC INTERSECTION UNDERSCORE BAD
       HASHBRACE EQUALS TYPE QUES BANG
       

%right UNION INTERSECTION

%start typ_ann
%start env

%type <Typedjs_syntax.annotation> typ_ann
%type <Typedjs_syntax.env_decl list> env

%%

args
  :  { [] }
  | arg_typ { [$1] }
  | arg_typ STAR args { $1 :: $3 }

pat :
  | REGEX { Sb_strPat.parse $startpos $1 }
  | ID { Sb_strPat.singleton $1 }
  | STRING { Sb_strPat.singleton $1 }

field :
  | pat COLON QUES typ { W.Maybe ($1, $4) }
  | pat COLON BANG typ { W.Present ($1, $4) }
  | pat COLON typ
      { let pat = $1 in
	if Sb_strPat.is_finite pat then
	  W.Present (pat, $3)
	else
	  W.Maybe (pat, $3) }
  | pat COLON UNDERSCORE { W.Absent $1 }
  | pat COLON BAD { W.Skull $1 }
  | STAR COLON typ { W.Star (Some $3) }
  | STAR COLON UNDERSCORE { W.Star None }

fields
  : { [] }
  | field { [$1] }
  | field COMMA fields { $1 :: $3 }
  | COMMA { [] }

arg_typ
  : ANY { W.Top }
  | INT { W.Prim Int }
  | NUM { W.Prim Num }
  | STR { W.Str }
  | BOOL { W.Bool }
  | TRUE { W.Prim True }
  | FALSE { W.Prim False }
  | UNDEF { W.Prim Undef }
  | NULL { W.Prim Null }
  | REGEX { W.Pat (Sb_strPat.parse $startpos $1) }
  | arg_typ UNION arg_typ { W.Union ($1, $3) }
  | arg_typ INTERSECTION arg_typ { W.Inter ($1, $3) }
  | LBRACE fields RBRACE { W.Ref (W.Object $2) }
  | LBRACE LBRACE fields RBRACE RBRACE { W.Source (W.SimpleObject $3) }
  | HASHBRACE fields RBRACE { W.Source (W.Object $2) }
  | LPAREN typ RPAREN { $2 }
  | TID { W.Id $1 }
  | ID { W.Syn $1 }
  | ID LANGLE typ RANGLE { W.App (W.Syn $1, $3) }

typ 
  : arg_typ { $1 }
  | args ARROW typ { W.Arrow (Some W.Top, $1, $3) }
  | LBRACK typ RBRACK args ARROW typ { W.Arrow (Some $2, $4, $6) }
  | FORALL ID LTCOLON typ DOT typ { W.Forall ($2, $4, $6) }
  | FORALL ID DOT typ { W.Forall ($2, W.Top, $4) }
  | REC ID DOT typ { W.Rec ($2, $4) }


annotation :
  | typ { ATyp $1 }
  | CHEAT typ { ACheat $2 }
  | UPCAST typ { AUpcast $2 }
  | DOWNCAST typ { ADowncast $2 }
  | CONSTRUCTOR typ { AConstructor $2 }
  | FORALL ID LTCOLON typ { ATypAbs ($2, $4) }
  | FORALL ID { ATypAbs ($2, W.Top) }
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

env_decl :
  | TYPE any_id LANGLE ID RANGLE EQUALS typ 
      { EnvType (($startpos, $endpos), $2, W.Lambda ($4, $7)) }
  | TYPE any_id EQUALS typ { EnvType (($startpos, $endpos), $2, $4) }
  | VAL ID COLON typ { EnvBind (($startpos, $endpos), $2, $4) }
  | ID COLON typ { EnvBind (($startpos, $endpos), $1, W.Ref $3) }
  | OPERATOR STRING COLON typ 
      { EnvBind (($startpos, $endpos), $2, W.remove_this $4) }

env_decls
  : { [] }
  | env_decl env_decls { $1 :: $2 }

env
  : env_decls EOF { $1 }

%%

%{

open Prelude
open Typedjs_syntax
module W = Typedjs_syntax.WritTyp

let rec remove_this op = match op with
  | W.Arrow (_, aa, r) -> W.Arrow (None, aa, r)
  | W.Inter (t1, t2) -> W.Inter (remove_this t1, remove_this t2)
  | W.Forall (x, s, t) -> W.Forall (x, s, remove_this t)
  | _ -> failwith "remove_this : illegal argument"

%}

%token <string> ID TID STRING REGEX
%token ARROW LPAREN RPAREN ANY STAR COLON EOF NUM UNION STR
       UNDEF BOOL LBRACE RBRACE COMMA VAL LBRACK RBRACK DOT OPERATOR
       UPCAST DOWNCAST FORALL LTCOLON IS LANGLE RANGLE
       CHEAT NULL TRUE FALSE REC INTERSECTION UNDERSCORE BAD
       HASHBRACE EQUALS TYPE QUES BANG TYPREC TYPLAMBDA THICKARROW
       COLONCOLON CARET LLBRACE RRBRACE REF

%right UNION INTERSECTION THICKARROW REF
%left LANGLE

%start typ_ann
%start env

%type <Typedjs_syntax.annotation> typ_ann
%type <Typedjs_syntax.env_decl list> env

%%

kind :
  | STAR { KStar }
  | LPAREN kind RPAREN { $2 }
  | kind THICKARROW kind { KArrow ([$1], $3) }

args
  :  { [] }
  | arg_typ { [$1] }
  | arg_typ STAR args { $1 :: $3 }

pat :
  | REGEX { (P.parse $startpos $1, true) }
  | ID { (P.singleton $1, false) }
  | STRING { (P.singleton $1, false) }

field :
  | pat COLON QUES typ { W.Maybe (fst2 $1, $4) }
  | pat COLON BANG typ { W.Present (fst2 $1, $4) }
  | pat COLON typ
      { let (pat, is_regex) = $1 in
  if is_regex then
          W.Maybe (pat, $3)
  else
         W.Present (pat, $3) }
  | pat COLON CARET typ { W.Inherited (fst2 $1, $4) }
  | pat COLON UNDERSCORE { W.Absent (fst2 $1) }
  | pat COLON BAD { W.Skull (fst2 $1) }
  | STAR COLON typ { W.Star (Some $3) }
  | STAR COLON UNDERSCORE { W.Star None }

fields
  : { [] }
  | field { [$1] }
  | field COMMA fields { $1 :: $3 }
  | COMMA { [] }

typ_list :
  |  { [] }
  | typ { [$1] }
  | typ COMMA typ_list { $1 :: $3 }

arg_typ
  : ANY { W.Top }
  | NUM { W.Prim "Num" }
  | STR { W.Str }
  | BOOL { W.Bool }
  | TRUE { W.Prim "True" }
  | FALSE { W.Prim "False" }
  | UNDEF { W.Prim "Undef" }
  | NULL { W.Prim "Null" }
  | REGEX { W.Pat (P.parse $startpos $1) }
  | arg_typ UNION arg_typ { W.Union ($1, $3) }
  | arg_typ INTERSECTION arg_typ { W.Inter ($1, $3) }
  | LBRACE fields RBRACE { W.Ref (W.Object $2) }
  | HASHBRACE fields RBRACE { W.Source (W.Object $2) }
  | LLBRACE fields RRBRACE { W.Object $2 }
  | LPAREN typ RPAREN { $2 }
  | TID { W.Id $1 }
  | ID { W.Syn $1 }
  | REF arg_typ { W.Ref $2 } 
  | arg_typ LANGLE typ_list RANGLE { W.App ($1, $3) }

typ 
  : arg_typ { $1 }
  | args ARROW typ { W.Arrow (Some W.Top, $1, $3) }
  | LBRACK typ RBRACK args ARROW typ { W.Arrow (Some $2, $4, $6) }
  | LBRACK RBRACK args ARROW typ { W.Arrow (None, $3, $5) }
  | FORALL ID LTCOLON typ DOT typ { W.Forall ($2, $4, $6) }
  | FORALL ID DOT typ { W.Forall ($2, W.Top, $4) }
  | REC ID DOT typ { W.Rec ($2, $4) }
  | TYPLAMBDA ID COLONCOLON kind DOT typ { W.Lambda ([($2, $4)], $6) }
  | TYPREC ID COLONCOLON kind DOT typ { W.Fix ($2, $4, $6) }


annotation :
  | typ { ATyp $1 }
  | CHEAT typ { ACheat $2 }
  | UPCAST typ { AUpcast $2 }
  | DOWNCAST typ { ADowncast $2 }
  | FORALL ID LTCOLON typ { ATypAbs ($2, $4) }
  | FORALL ID { ATypAbs ($2, W.Top) }
  | LBRACK typ RBRACK { ATypApp $2 }
  | IS typ { AAssertTyp $2 }

typ_ann :
  | annotation EOF { $1 }

any_id :
  | ID { $1 }
  | STR { "Str" }
  | UNDEF { "Undef" }
  | BOOL { "Bool" }
  | NUM { "Num" }

id_list :
  | { [] }
  | ID { [$1] }
  | ID COMMA id_list { $1 :: $3 }

env_decl :
  | TYPE any_id LANGLE id_list RANGLE EQUALS typ 
      { EnvType (($startpos, $endpos), $2,
     W.Lambda (List.map (fun x -> (x, KStar)) $4, $7)) }
  | TYPE any_id EQUALS typ { EnvType (($startpos, $endpos), $2, $4) }
  | VAL ID COLON typ { EnvBind (($startpos, $endpos), $2, $4) }
  | ID COLON typ { EnvBind (($startpos, $endpos), $1, W.Ref $3) }
  | OPERATOR STRING COLON typ 
      { EnvBind (($startpos, $endpos), $2, remove_this $4) }

env_decls
  : { [] }
  | env_decl env_decls { $1 :: $2 }

env
  : env_decls EOF { $1 }

%%

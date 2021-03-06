%{

open Prelude
open Typedjs_syntax
open TypImpl
module W = Typedjs_syntax.WritTyp

let rec remove_this op = match op with
  | W.Arrow (_, aa, v, r) -> W.Arrow (None, aa, v, r)
  | W.Inter (t1, t2) -> W.Inter (remove_this t1, remove_this t2)
  | W.Forall (x, s, t) -> W.Forall (x, s, remove_this t)
  | W.Ref (W.Object (W.Present(_, t)::fields)) -> remove_this t
  | W.With(t, f) -> W.With(remove_this t, f)
  | _ -> failwith "remove_this : illegal argument"

let wrapArrow arrTyp =
  W.Ref( W.Object ([W.Present(P.singleton "-*- code -*-", arrTyp);
                    W.Present(proto_pat, W.Id "Object"); (* ADDING THIS CAUSES AN ERROR "Object is unbound" *)
                    W.Present(P.singleton "prototype", W.Id "Ext");
                    W.Star(Some (W.Id "Ext"))]))

let matchArrow t = match t with
  | W.Ref(W.Object([W.Present(code, t);
                    W.Present(proto, W.Id "Object");
                    W.Present(prototypePat, W.Id "Ext");
                    W.Star(Some (W.Id "Ext"))])) -> Some t
  | _ -> None

let rec pushForallFunction typ = match typ with
  | W.Forall (var, bound, t) -> begin
    match (matchArrow t) with
    | Some (W.Arrow _ as arrTyp)
    | Some (W.Forall _ as arrTyp) ->
      wrapArrow (W.Forall(var, bound, arrTyp))
    | Some _
    | None -> Printf.eprintf "Found a forall that couldn't be pushed %s\n" (W.print_typ typ); typ
  end
  | W.With(t, f) -> W.With(pushForallFunction t, f)
  | _ -> typ

let rec pushIntersectFunction typ = match typ with
  | W.Inter(t1, t2) -> begin match (matchArrow t1, matchArrow t2) with
    | Some t1, Some t2 -> wrapArrow (W.Inter (t1, t2))
    | _, _ -> Printf.eprintf "Didn't get two function objects to pushIntersectFunction: \n%s\nand\n%s\n"
      (W.print_typ t1) (W.print_typ t2); typ
  end
  | _ -> typ
%}

%token <string> ID TID STRING REGEX PRIM
%token ARROW LPAREN RPAREN ANY STAR COLON EOF UNION STR AND
       BOOL LBRACE RBRACE COMMA VAL LBRACK RBRACK DOT OPERATOR SEMI
       UPCAST DOWNCAST FORALL LTCOLON IS LANGLE RANGLE
       CHEAT REC INTERSECTION UNDERSCORE BAD WITH THIS
       HASHBRACE EQUALS TYPE QUES BANG TYPREC TYPLAMBDA THICKARROW
       COLONCOLON CARET LLBRACE RRBRACE REF PRIMITIVE DOTS
       CONSTRUCTOR PROTOTYPE INSTANCE

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
  :  { ([], None) }
  | arg_typ { ([$1], None) }
  | arg_typ DOTS { ([], Some $1) }
  | arg_typ STAR args { let (args, var) = $3 in (($1 :: args), var) }

pat :
  | REGEX { (P.parse $startpos $1, true) }
  | any_id { (P.singleton $1, false) }
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

obj_ref_typ:
  | LBRACE fields RBRACE { W.Ref (W.Object $2) }
  | HASHBRACE fields RBRACE { W.Source (W.Object $2) }
  | LBRACE typ WITH fields RBRACE { W.With($2, $4) }


arg_typ
  : ANY { W.Top }
  | PRIM { W.Prim $1 }
  | STR { W.Str }
  | BOOL { W.Bool }
  | REGEX { W.Pat (P.parse $startpos $1) }
  | arg_typ UNION arg_typ { W.Union ($1, $3) }
  | arg_typ INTERSECTION arg_typ { pushIntersectFunction (W.Inter ($1, $3)) }
  | LPAREN typ RPAREN { $2 }
  | LLBRACE fields RRBRACE { W.Object $2 }
  | obj_ref_typ { $1 }
  | TID { W.Id $1 }
  | ID { W.Syn $1 }
  | REF arg_typ { W.Ref $2 } 
  | arg_typ LANGLE typ_list RANGLE { W.App ($1, $3) }

typ :
  | arg_typ { $1 }
  | args ARROW typ { let (args, var) = $1 in wrapArrow (W.Arrow (Some W.Top, args, var, $3)) }
  | LBRACK typ RBRACK args ARROW typ { let (args, var) = $4 in wrapArrow (W.Arrow (Some $2, args, var, $6)) }
  | LBRACK THIS LPAREN typ RPAREN RBRACK args ARROW typ 
      { let (args, var) = $7 in wrapArrow (W.Arrow (Some (W.This $4), args, var, $9)) }
  | LBRACK RBRACK args ARROW typ { let (args, var) = $3 in wrapArrow (W.Arrow (None, args, var, $5)) }
  | args THICKARROW typ { let (args, var) = $1 in W.Arrow (Some W.Top, args, var, $3) }
  | LBRACK typ RBRACK args THICKARROW typ { let (args, var) = $4 in W.Arrow (Some $2, args, var, $6) }
  | LBRACK THIS LPAREN typ RPAREN RBRACK args THICKARROW typ 
      { let (args, var) = $7 in W.Arrow (Some (W.This $4), args, var, $9) }
  | LBRACK RBRACK args THICKARROW typ { let (args, var) = $3 in W.Arrow (None, args, var, $5) }
  | FORALL ID LTCOLON typ DOT typ { pushForallFunction (W.Forall ($2, $4, $6)) }
  | FORALL ID DOT typ { pushForallFunction (W.Forall ($2, W.Top, $4)) }
  | FORALL ID LTCOLON typ COLON typ {  (W.Forall ($2, $4, $6)) } (* Allow for not pushing forall inward *)
  | FORALL ID COLON typ {  (W.Forall ($2, W.Top, $4)) }
  | REC ID DOT typ { W.Rec ($2, $4) }
  | TYPLAMBDA args=separated_nonempty_list(COMMA, separated_pair(ID, COLONCOLON, kind)) DOT typ=typ
    { W.Lambda (args, typ) }
  | TYPREC ID COLONCOLON kind DOT typ { W.Fix ($2, $4, $6) }


annotation :
  | typ { ATyp $1 }
  | CHEAT typ { ACheat $2 }
  | UPCAST typ { AUpcast $2 }
  | DOWNCAST typ { ADowncast $2 }
  | FORALL ID LTCOLON typ { ATypAbs ($2, $4) }
  | FORALL ID { ATypAbs ($2, W.Top) }
  | LBRACK typ RBRACK { ATypApp [$2] }
  | ID BANG typs=nonempty_list(delimited(LBRACK, typ, RBRACK)) { ATypApp typs }
  | IS typ { AAssertTyp $2 }

typ_ann :
  | annotation EOF { $1 }

any_id :
  | ID { $1 }
  | PRIM { $1 }
  | STR { "Str" }
  | BOOL { "Bool" }
  | PROTOTYPE { "prototype" }
  | CONSTRUCTOR { "constructor" }
  | INSTANCE { "instance" }

id_list :
  | { [] }
  | ID { [$1] }
  | ID COMMA id_list { $1 :: $3 }

env_decl :
  | TYPE CONSTRUCTOR c_id=any_id EQUALS c_typ=typ
      AND PROTOTYPE p_id=any_id EQUALS p_typ=obj_ref_typ
      AND INSTANCE i_id=any_id EQUALS i_typ=obj_ref_typ
    { ObjectTrio (Pos.real ($startpos, $endpos), (c_id, c_typ), (p_id, p_typ), (i_id, i_typ)) }
  | TYPE any_id LANGLE id_list RANGLE EQUALS typ 
      { EnvType (Pos.real ($startpos, $endpos), $2,
     W.Lambda (List.map (fun x -> (x, KStar)) $4, $7)) }
  | TYPE any_id EQUALS typ { EnvType (Pos.real ($startpos, $endpos), $2, $4) }
  | VAL ID COLON typ { EnvBind (Pos.real ($startpos, $endpos), $2, $4) }
  | ID COLON typ { EnvBind (Pos.real ($startpos, $endpos), $1, W.Ref $3) }
  | OPERATOR STRING COLON typ 
      { EnvBind (Pos.real ($startpos, $endpos), $2, remove_this $4) }
  | PRIMITIVE PRIM { EnvPrim (Pos.real ($startpos, $endpos), $2) }

rec_env_decl : 
  | env_decl { $1 }
  | REC recs=separated_nonempty_list(AND, env_decl) { RecBind(recs) }

env_decls
  : { [] }
  | rec_env_decl SEMI? env_decls { $1 :: $3 }

env
  : env_decls EOF { $1 }

%%

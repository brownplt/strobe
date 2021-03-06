%{
open Prelude
open Typedjs_syntax
open JavaScript_syntax
open Sb_desugar


let parse_annotation (pos, end_p) str =
  let lexbuf = Lexing.from_string str in
    lexbuf.Lexing.lex_start_p <- pos;
    lexbuf.Lexing.lex_curr_p <- pos;
    try
      Typedjs_parser.typ_ann Typedjs_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "error lexing annotation at %s"
                       (Pos.rangeToString
                          lexbuf.Lexing.lex_curr_p lexbuf.Lexing.lex_curr_p))
      |  Typedjs_parser.Error ->
           failwith (sprintf "error parsing annotation at %s"
                       (Pos.rangeToString
                          lexbuf.Lexing.lex_curr_p lexbuf.Lexing.lex_curr_p))


%}

%token <string> HINT

%token <int> INT
%token <float> NUM
%token <string> STRING
%token <bool> BOOL
%token <Prelude.id> ID
%token UNDEFINED NULL FUNC LET LBRACE RBRACE LPAREN RPAREN LBRACK
  RBRACK EQUALS COMMA DEREF REF COLON COLONEQ PRIM IF ELSE SEMI
  LABEL BREAK TRY CATCH FINALLY THROW LLBRACK RRBRACK EQEQEQUALS TYPEOF
  AMPAMP PIPEPIPE BANGEQEQUALS SOURCE REC LANGLE RANGLE
  UPCAST


%token EOF
%left COLONEQ
%left LPAREN
%left PIPEPIPE
%left AMPAMP
%left EQEQEQUALS BANGEQEQUALS
%left LBRACK
%left LANGLE

/* http://stackoverflow.com/questions/1737460/
   how-to-find-shift-reduce-conflict-in-this-yacc-file */

%type <Typedjs_syntax.exp> prog
%type <Typedjs_syntax.exp -> Typedjs_syntax.exp> env

%start prog
%start env


%%

const :
 | NUM { JavaScript_syntax.CNum $1 }
 | INT {  JavaScript_syntax.CInt $1 }
 | STRING {  JavaScript_syntax.CString $1 }
 | UNDEFINED { JavaScript_syntax.CUndefined }
 | NULL { JavaScript_syntax.CNull }
 | BOOL { JavaScript_syntax.CBool $1 }

prop :
 | STRING COLON exp { ($1, $3) }
 | ID COLON exp { ($1, $3) }

props :
 | { [] }
 | prop { [$1] }
 | prop COMMA props { $1 :: $3 }

exps :
 | { [] }
 | seq_exp { [$1] }
 | seq_exp COMMA exps { $1 :: $3 }

ids :
 | { [] }
 | ID { [$1] }
 | ID COMMA ids { $1 :: $3 }

func :
 | FUNC LPAREN ids RPAREN HINT LBRACE seq_exp RBRACE
   { let p = ($startpos, $endpos) in
     let t = match parse_annotation p $5 with
       | ATyp t -> t
       | _ -> failwith "expected a type on the function, got something else" in
     let p = Pos.real p in
     EAssertTyp (p, desugar_typ p t, 
                 EFunc (p, $3, 
                        { func_owned = IdSet.empty; func_loop = false }, 
                        $7)) }

atom :
 | const { EConst (Pos.real ($startpos, $endpos), $1) }
 | ID { EId (Pos.real ($startpos, $endpos), $1) }
 | LBRACE props RBRACE 
   { EObject (Pos.real ($startpos, $endpos), $2) }
 | LBRACE seq_exp RBRACE
   { $2 }
 | LPAREN seq_exp RPAREN { $2 }
 | func { $1 }
 | UPCAST HINT atom
   { let t = match parse_annotation ($startpos, $endpos) $2 with
     | ATyp t -> typ t
     | _ -> failwith "Upcast expected a typ" in
     ESubsumption (Pos.real ($startpos, $endpos), t, $3) }
 | DEREF atom
   { EDeref (Pos.real ($startpos, $endpos), $2) }
 | REF atom
   { ERef (Pos.real ($startpos, $endpos), RefCell, $2) }
 | SOURCE atom
   { ERef (Pos.real ($startpos, $endpos), SourceCell, $2) }
 | TYPEOF atom
     { EPrefixOp (Pos.real ($startpos, $endpos), "typeof", $2) }


exp :
 | atom { $1 }
 | exp LANGLE HINT RANGLE
     { let t = match parse_annotation ($startpos, $endpos) $3 with
       | ATyp t -> t
       | _ -> failwith "Expected type in application" in
       ETypApp (Pos.real ($startpos, $endpos), $1, typ t) }
 | exp LPAREN exps RPAREN 
   { EApp (Pos.real ($startpos, $endpos), $1, $3) }
 | PRIM LPAREN STRING COMMA seq_exp COMMA seq_exp RPAREN
   { EInfixOp (Pos.real ($startpos, $endpos), $3, $5, $7) }
 | PRIM LPAREN STRING COMMA seq_exp RPAREN
   { EPrefixOp (Pos.real ($startpos, $endpos), $3, $5) }
 | exp COLONEQ exp
   { ESetRef (Pos.real ($startpos, $endpos), $1, $3) }
 | exp EQEQEQUALS exp
     { EInfixOp (Pos.real ($startpos, $endpos), "stx=", $1, $3) }
 | exp BANGEQEQUALS exp
     { let p = Pos.real ($startpos, $endpos) in
         EIf (p, EInfixOp (p, "stx=", $1, $3),
              EConst (p, CBool false),
              EConst (p, CBool true)) }
 | exp LBRACK seq_exp EQUALS seq_exp RBRACK
   { EUpdate (Pos.real ($startpos, $endpos), $1, $3, $5) }
 | exp LBRACK seq_exp RBRACK
   { EBracket (Pos.real ($startpos, $endpos), $1, $3) }

 | exp AMPAMP exp
     { EIf (Pos.real ($startpos, $endpos), $1, 
            $3,
            EConst (Pos.real ($startpos, $endpos), CBool false)) }
 | exp PIPEPIPE exp
     { let p = Pos.real ($startpos, $endpos) in
         ELet (p, "%or", $1,
               EIf (p, EId (p, "%or"), EId (p, "%or"), $3)) }


cexp :
 | exp { $1 }
 | LABEL ID COLON cexp
     { ELabel (Pos.real ($startpos, $endpos), $2, $4) } 
 | BREAK ID cexp
   { EBreak (Pos.real ($startpos, $endpos), $2, $3) }
 | THROW cexp
   { EThrow (Pos.real ($startpos, $endpos), $2) }
 | TRY LBRACE seq_exp RBRACE CATCH LPAREN ID RPAREN LBRACE seq_exp RBRACE
   { ETryCatch (Pos.real ($startpos, $endpos), $3, $7, $10) }
 | TRY LBRACE seq_exp RBRACE FINALLY LBRACE seq_exp RBRACE
   { ETryFinally (Pos.real ($startpos, $endpos), $3, $7) }

seq_exp :
 | cexp { $1 }
 | IF LPAREN seq_exp RPAREN cexp ELSE cexp
     { EIf (Pos.real ($startpos, $endpos), $3, $5, $7) }
 | LET LPAREN ID EQUALS seq_exp RPAREN seq_exp
   { ELet (Pos.real ($startpos, $endpos), $3, $5, $7) }
 | REC LPAREN ID HINT EQUALS seq_exp RPAREN seq_exp
   { let t = match parse_annotation ($startpos, $endpos) $4 with
     | ATyp t -> typ t
     | _ -> failwith "Expected a type for rec, something else" in
     ERec (Pos.real ($startpos, $endpos), [($3, t, $6)], $8) }
 | cexp SEMI seq_exp
   { ESeq (Pos.real ($startpos, $endpos), $1, $3) }


env :
 | EOF
     { fun x -> x }
 | LET LLBRACK ID RRBRACK EQUALS seq_exp env
     { fun x -> 
         ELet (Pos.real ($startpos, $endpos), "[[" ^ $3 ^ "]]", $6, $7 x) }
 | LBRACE seq_exp RBRACE env
     { fun x -> ESeq (Pos.real ($startpos, $endpos), $2, $4 x) }

prog :
 | seq_exp EOF { $1 }
%%

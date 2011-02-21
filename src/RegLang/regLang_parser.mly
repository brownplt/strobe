%{

open Prelude
open RegLang_syntax

%}

%token <string> STRING
%token <char> CHAR
%token LPAREN RPAREN LBRACK RBRACK CARET STAR UNDERSCORE PIPE DOT LBRACE RBRACE
       COMMA LTCOLON LTSLASHCOLON SEMI EOF

%start regex_tests
%start regex

%type <(Prelude.pos * RegLang_syntax.regex *
         RegLang_syntax.regex * bool) list> regex_tests
%type <RegLang_syntax.regex> regex

%%

atom :
  | STRING { String $1 }
  | CHAR { InSet (CharSet.singleton $1) }
  | LPAREN cat RPAREN { $2 }
  | DOT { NotInSet CharSet.empty }

star :
  | atom { $1 }
  | atom STAR { Star $1 }

cat :
  | star { $1 }
  | star cat { Concat ($1, $2) }
  | atom PIPE atom { Alt($1, $3) }

regex :
  | cat EOF { $1 }

regex_tests :
  | EOF { [] }
  | cat LTCOLON cat SEMI regex_tests { (($startpos, $endpos), $1, $3, true) :: $5 }
  | cat LTSLASHCOLON cat SEMI regex_tests { (($startpos, $endpos), $1, $3, false) :: $5 }


%%

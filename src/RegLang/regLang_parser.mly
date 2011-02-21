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

%type <(RegLang_syntax.regex * RegLang_syntax.regex * bool) list> regex_tests
%type <RegLang_syntax.regex> regex

%%

atom :
  | STRING { String $1 }
  | CHAR { InSet (CharSet.singleton $1) }
  | LPAREN regex RPAREN { $2 }
  | DOT { NotInSet CharSet.empty }

regex_cat :
  | atom { $1 }
  | atom STAR { Star $1 }

regex :
  | regex_cat { $1 }
  | regex_cat regex { Concat ($1, $2) }

regex_tests :
  | EOF { [] }
  | regex LTCOLON regex SEMI regex_tests { ($1, $3, true) :: $5 }
  | regex LTSLASHCOLON regex SEMI regex_tests { ($1, $3, false) :: $5 }


%%

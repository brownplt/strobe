%{

open Prelude
open RegLang_syntax

%}

%token <string> STRING
%token <char> CHAR
%token LPAREN RPAREN STAR PIPE DOT BANG
       LTCOLON LTSLASHCOLON SEMI EOF LBRACK RBRACK
       HYPHEN CARET

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
  | LBRACK chardescs RBRACK { $2 }

star :
  | atom { $1 }
  | atom STAR { Star $1 }

alt :
  | cat PIPE cat { Alt($1, $3) }

cat :
  | star { $1 }
  | star cat { Concat ($1, $2) }
  | alt { $1 }

chardescs :
  | chardesc { $1 }
  | chardesc chardescs { Alt($1, $2) }

chardesc :
  | CHAR HYPHEN CHAR { InSet (build_range $1 $3) }
  | CHAR { InSet (CharSet.singleton $1) }
  | CARET chardesc { match $2 with
                       | InSet chs -> NotInSet chs
                       | _ -> failwith ("Multiple negation?") }

regex :
  | cat EOF { $1 }

regex_tests :
  | EOF { [] }
  | cat LTCOLON cat SEMI regex_tests { (($startpos, $endpos), $1, $3, true) :: $5 }
  | cat LTSLASHCOLON cat SEMI regex_tests { (($startpos, $endpos), $1, $3, false) :: $5 }


%%

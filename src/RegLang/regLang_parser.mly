%{

open Prelude
open RegLang_syntax

%}

%token <string> STRING
%token <char> CHAR
%token LPAREN RPAREN STAR PIPE DOT
       LTCOLON LTSLASHCOLON SEMI EOF LBRACK RBRACK
       HYPHEN CARET

%left PIPE

%start regex_tests
%start regex

%type <(Prelude.pos * RegLang_syntax.regex *
         RegLang_syntax.regex * bool) list> regex_tests
%type <RegLang_syntax.regex> regex

%%

atom :
  | STRING { String $1 }
  | CHAR { InSet (CharSet.singleton $1) }
  | LPAREN alt RPAREN { $2 }
  | DOT { NotInSet CharSet.empty }
  | LBRACK chardescs RBRACK { $2 }

negate :
  | atom { $1 }
  | CARET atom { Negate $2 }

star :
  | negate { $1 }
  | negate STAR { Star $1 }

alt :
  | cat { $1 }
  | alt PIPE alt { Alt($1, $3) }

cat :
  | star { $1 }
  | star cat { Concat ($1, $2) }

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
  | alt EOF { $1 }

regex_tests :
  | EOF { [] }
  | alt LTCOLON alt SEMI regex_tests { (($startpos, $endpos), $1, $3, true) :: $5 }
  | alt LTSLASHCOLON alt SEMI regex_tests { (($startpos, $endpos), $1, $3, false) :: $5 }


%%

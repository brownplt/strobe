%{

  open Jsdoc_syntax
  
  let empty_func = {
    new_ = None;
    this = None;
    args = [];
    vararg = None;
    result = None
  }

%}

%token PARAM RETURN LBRACE RBRACE DOT QUES BANG FUNCTION LPAREN RPAREN
%token COLON LANGLE RANGLE PIPE COMMA STAR EOF ALL
%token <string> ID

%start <Jsdoc_syntax.typ> annotation

%%

typ_list :
  | { [] }
  | typ { [$1] }
  | typ COMMA typ_list { $1 :: $3 }

typ :
  | FUNCTION LPAREN typ_list RPAREN 
      { Func { empty_func with args = $3 } }
  | FUNCTION LPAREN typ_list RPAREN COLON typ
      { Func { empty_func with args = $3; result = Some $6 } }
  | STAR
      { All }
  | ID 
      { Var $1 }
  | ID DOT LANGLE typ_list RANGLE
      { App ($1, $4) }

params :
  | { [] }
  | PARAM  LBRACE typ RBRACE params { $3 :: $5 }

return :
  | { None }
  | RETURN LBRACE typ RBRACE { Some $3 }

annotation :
  | params return EOF { Func { empty_func with args = $1; result = $2 } }

%%

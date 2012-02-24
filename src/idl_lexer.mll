{
  open Lexing
  open Idl_parser


  let keyword_table = Hashtbl.create 53
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [ 
                "scriptable" , SCRIPTABLE ;
                "module" , MODULE ;
                "in" , IN ;
                "out" , OUT ;
                "inout" , INOUT ;
                "true" , TRUE ;
                "false" , FALSE ;
                "noscript" , NOSCRIPT ;
                "notxpcom" , NOTXPCOM ;
                "retval" , RETVAL ;
                "optional_argc" , OPTIONAL_ARGC ;
                (* "PRUnichar" , PRUnichar ; *)
                (* "PRUint32" , PRUint32 ; *)
                (* "PRInt32" , PRInt32 ; *)
                (* "PRUint16" , PRUint16 ; *)
                (* "PRInt16" , PRInt16 ; *)
                "size_is" , SIZE_IS ;
                "inherit" , INHERIT ;
                "interface" , INTERFACE ;
                "static" , STATIC ;
                "enum" , ENUM ;
                "short" , SHORT ;
                "long" , LONG ;
                "boolean" , BOOLEAN ;
                "Clamp" , CLAMP ;
                "byte" , BYTE ;
                "octet" , OCTET ;
                "float" , FLOAT ;
                "double" , DOUBLE ;
                (* "DOMString" , DOMSTRING ; *)
                (* "Date" , DATE ; *)
                "any" , ANY ;
                "unsigned" , UNSIGNED ;
                "readonly" , READONLY ;
                "void" , VOID ;
                "optional" , OPTIONAL ;
                "attribute" , ATTRIBUTE ;
                "typedef" , TYPEDEF ;
                (* "object" , OBJECT ; *)
                "exception" , EXCEPTION ;
                "const" , CONST ;
                "raises" , RAISES ;
                "legacycaller" , LEGACYCALLER ;
                "getter" , GETTER ;
                "setter" , SETTER ;
                "creator" , CREATOR ;
                "deleter" , DELETER ;
                "NoInterfaceObject" , NOINTERFACEOBJECT ;
                "OverrideBuiltins" , OVERRIDEBUILTINS ;
                "PutForwards" , PUTFORWARDS ;
                "implements" , IMPLEMENTS ;
                "stringifier" , STRINGIFIER ;
                "NamedConstructor" , NAMEDCONSTRUCTOR ;
                "Constructor" , CONSTRUCTOR ;
                "ReplaceableNamedProperties" , REPLACEABLENAMEDPROPERTIES ;
                "Replaceable" , REPLACEABLE ;
                "Unforgeable" , UNFORGEABLE ;
                "dictionary" , DICTIONARY ;
                "Callback" , CALLBACK ;
                "TreatNullAs" , TREATNULLAS ;
                "FunctionOnly" , FUNCTIONONLY ;
                "AllowAny" , ALLOWANY ;
                "partial" , PARTIAL ;
                "sequence" , SEQUENCE ;
                "implicit_jscontext" , IMPLICIT_JSCONTEXT
              ]
}


let ident = ['a'-'z' 'A'-'Z' '$' '_' '%']([ 'a'-'z' 'A'-'Z' '0'-'9' '$' '_']*)

let blank = [ ' ' '\t' '\r' ]

let hex = ['0'-'9' 'A'-'f' 'a'-'f']

let escape_sequence = [^ '\r' '\n'] | ('x' hex hex) | ('u' hex hex hex hex)

let double_quoted_string_char = [^ '\r' '\n' '"' '\\'] | ('\\' escape_sequence)

let hex4 = hex hex hex hex
let hex8 = hex hex hex hex hex hex hex hex 
let hex12 = hex hex hex hex hex hex hex hex hex hex hex hex 
let uuid = hex8 '-' hex4 '-' hex4 '-' hex4 '-' hex12

let integer = '-'?(('0'(['0'-'7']*|['X' 'x']['0'-'9' 'A'-'F' 'a'-'f']+))|(['1'-'9']['0'-'9']*))
let float = '-'?((['0'-'9']+'.'['0'-'9']*|['0'-'9']*'.'['0'-'9']+)(['E' 'e']['+' '-']?['0'-'9']+)?|['0'-'9']+['E' 'e']['+' '-']?['0'-'9']+)

rule token = parse
   | blank+ { token lexbuf }
   | '\n' { new_line lexbuf; token lexbuf }
   | '\r' { new_line lexbuf; token lexbuf }
   | "\r\n" { new_line lexbuf; token lexbuf }
   | "/*" { block_comment lexbuf }
   | "%{" [^ '\r' '\n']* { cpp_comment lexbuf }
   | "//"[^ '\r' '\n']* [ '\r' '\n' ] { new_line lexbuf; token lexbuf }
   | "..." { DOTDOTDOT }
   | "(" { LPAREN }
   | ")" { RPAREN }
   | "{" { LBRACE }
   | "}" { RBRACE }
   | "[" { LBRACK }
   | "]" { RBRACK }
   | "[]" { LRBRACK }
   | "," { COMMA }
   | ":" { COLON }
   | "::" { COLONCOLON }
   | "?" { QUES }
   | ";" { SEMI }
   | "=" { EQUALS }
   | "uuid" blank* '(' blank* (uuid as u) blank* ")" { UUID u }
   | "#include" { INCLUDE }
   | '\"' (double_quoted_string_char * as s) '\"' { STRING s }
   | "<" { LANGLE }
   | ">" { RANGLE }
   | integer as x { INTLIT (Int64.of_string x) }
   | float as f { FLOATLIT (float_of_string f) }
   | '|' { BAR }
   | '^' { XOR }
   | '&' { AND }
   | "<<" { SHLEFT }
   | ">>" { SHRIGHT }
   | '+' { PLUS }
   | '-' { MINUS }
   | '~' { TILDE }
   | '*' { TIMES }
   | '/' { DIVIDE }
   | '%' { MOD }
   | eof { EOF }
   | "native" blank+ (ident as id) blank* "(" ([^ '(' ')' '\n']* as natid) ")" { NATIVE (id, natid) }
   | ident as id { 
     try
       Hashtbl.find keyword_table id
     with Not_found ->
       ID (Id.id_of_string id) 
   }




and block_comment = parse
  | "*/" { token lexbuf }
  | '*' { block_comment lexbuf }
  | "\r\n" { new_line lexbuf; block_comment lexbuf }
  | [ '\n' '\r' ]  { new_line lexbuf; block_comment lexbuf }
  | [^ '\n' '\r' '*'] { block_comment lexbuf }

and cpp_comment = parse
  | "%}" [^ '\r' '\n']* { token lexbuf }
  | "\r\n" { new_line lexbuf; cpp_comment lexbuf }
  | [ '\n' '\r' ]  { new_line lexbuf; cpp_comment lexbuf }
  | [^ '\n' '\r'] { cpp_comment lexbuf }

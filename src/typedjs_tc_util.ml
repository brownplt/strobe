open Prelude
open Typedjs_syntax

exception Not_value of string

let tc_const (const : JavaScript_syntax.const) = match const with
    JavaScript_syntax.CString _ -> TPrim Str 
  | JavaScript_syntax.CRegexp _ -> raise (Not_value "RegExps are awkward") 
  | JavaScript_syntax.CNum _ -> TPrim Num 
  | JavaScript_syntax.CInt _ -> TPrim Int 
  | JavaScript_syntax.CBool _ -> typ_bool
  | JavaScript_syntax.CNull -> TPrim Null 
  | JavaScript_syntax.CUndefined -> TPrim Undef 

let typ_of_value (exp : exp) : typ = 
  let mk_field f (name, exp) =
    ((RegLang.String name, RegLang.fsm_of_regex (RegLang.String name)),
      f exp) in
  let rec f e = match e with
    | EObject (_, fields) -> 
       TObject (map (mk_field f) fields)
    | EConst (_, c) -> tc_const c
    | EFunc (_, _, t, _) -> t
    | ERef (_, RefCell, e') -> TRef begin match e' with
        | EObject (_, fields) -> 
            TObject (map (mk_field f) fields)
        | EConst (_, c) -> tc_const c
        | EFunc (_, _, t, _) -> t
        | _ -> raise (Not_value  (FormatExt.to_string Pretty.p_exp e'))
      end
    | _ -> raise (Not_value (FormatExt.to_string Pretty.p_exp e)) in
    f exp

open Prelude
open Typedjs_syntax

exception Not_value of string

let tc_const (const : JavaScript_syntax.const) = match const with
    JavaScript_syntax.CString s -> 
      let regex = RegLang_syntax.String s in
        TRegex (regex, RegLang.fsm_of_regex regex)
  | JavaScript_syntax.CRegexp _ -> TSyn "RegExp"
  | JavaScript_syntax.CNum _ -> TPrim Num 
  | JavaScript_syntax.CInt _ -> TPrim Int 
  | JavaScript_syntax.CBool _ -> typ_bool
  | JavaScript_syntax.CNull -> TPrim Null 
  | JavaScript_syntax.CUndefined -> TPrim Undef 

let typ_of_value (exp : exp) : typ = 
  let mk_field f (name, exp) =
    let re = RegLang_syntax.String name in
    let fsm = RegLang.fsm_of_regex re in
      ((re, fsm), PPresent (f exp)) in
  let rec f e = match e with
    | EObject (_, fields) -> 
        mk_object_typ (map (mk_field f) fields) None (TSyn "Object")
    | EConst (_, c) -> tc_const c
    | EFunc (_, _, fi, _) -> (match fi.func_typ with | WrittenTyp t -> t)
    | ERef (_, RefCell, e') -> TRef begin match e' with
        | EObject (_, fields) -> 
            mk_object_typ (map (mk_field f) fields) None (TSyn "Object")
        | EConst (_, c) -> tc_const c
        | EFunc (_, _, fi, _) -> 
	  (match fi.func_typ with | WrittenTyp t -> t)
        | _ -> raise (Not_value  (FormatExt.to_string Pretty.p_exp e'))
      end
    | _ -> raise (Not_value (FormatExt.to_string Pretty.p_exp e)) in
    f exp

open Prelude
open Typedjs_syntax

exception Not_value of string

let tc_const (const : JavaScript_syntax.const) = match const with
  | JavaScript_syntax.CString s -> TRegex (Sb_strPat.singleton s)
  | JavaScript_syntax.CRegexp _ -> TId "RegExp"
  | JavaScript_syntax.CNum _ -> TPrim Num 
  | JavaScript_syntax.CInt _ -> TPrim Int 
  | JavaScript_syntax.CBool _ -> typ_bool
  | JavaScript_syntax.CNull -> TPrim Null 
  | JavaScript_syntax.CUndefined -> TPrim Undef 

let typ_of_value (exp : exp) : typ = 
  let mk_field f (name, exp) = (Sb_strPat.singleton name, PPresent (f exp)) in
  let rec f e = match e with
    | EObject (_, fields) -> 
      (* TODO: Everything else should be absent *)
      TObject (map (mk_field f) fields, TId "Object")
    | EConst (_, c) -> tc_const c
    | EFunc (_, _, fi, _) -> fi.func_typ
    | ERef (_, RefCell, e') -> TRef begin match e' with
        | EObject (_, fields) -> 
	  (* TODO: as above *)
	  TObject (map (mk_field f) fields, TId "Object")
        | EConst (_, c) -> tc_const c
        | EFunc (_, _, fi, _) -> fi.func_typ
        | _ -> raise (Not_value  (FormatExt.to_string Pretty.p_exp e'))
      end
    | _ -> raise (Not_value (FormatExt.to_string Pretty.p_exp e)) in
    f exp

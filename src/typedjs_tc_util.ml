open Prelude
open Typedjs_syntax
open Typedjs_types

let tc_const (const : JavaScript_syntax.const) = match const with
    JavaScript_syntax.CString _ -> typ_str
  | JavaScript_syntax.CRegexp _ -> typ_regexp
  | JavaScript_syntax.CNum _ -> typ_num
  | JavaScript_syntax.CInt _ -> typ_int
  | JavaScript_syntax.CBool _ -> typ_bool
  | JavaScript_syntax.CNull -> typ_null
  | JavaScript_syntax.CUndefined -> typ_undef

exception Not_value of string

let typ_of_value (exp : exp) : typ = 
  let rec f e = match e with
    | EObject (_, fields) -> TObject (map (second2 f) fields)
    | EConst (_, c) -> tc_const c
    | EFunc (_, _, t, _) -> t
    | ERef (_, RefCell, e') -> TRef begin match e' with
        | EObject (_, fields) -> TObject (map (second2 f) fields)
        | EConst (_, c) -> tc_const c
        | EFunc (_, _, t, _) -> t
        | _ -> raise (Not_value  (FormatExt.to_string Pretty.p_exp e'))
      end
    | _ -> raise (Not_value (FormatExt.to_string Pretty.p_exp e)) in
    f exp

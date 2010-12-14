open Prelude
open Typedjs_syntax
open Typedjs_types
open Typedjs_env

let tc_const (const : JavaScript_syntax.const) = match const with
    JavaScript_syntax.CString s -> TStrSet [s]
  | JavaScript_syntax.CRegexp _ -> typ_regexp
  | JavaScript_syntax.CNum _ -> typ_num
  | JavaScript_syntax.CInt _ -> typ_int
  | JavaScript_syntax.CBool _ -> typ_bool
  | JavaScript_syntax.CNull -> typ_null
  | JavaScript_syntax.CUndefined -> typ_undef

exception Not_value of string
exception Not_wf_typ of string

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

let rec typ_of_value_init (exp : exp) : typ =
  match exp with 
    | EConst (_, JavaScript_syntax.CString s) -> TConstr ("Str", [])
    | ESubsumption (_, t, e) -> ignore (typ_of_value_init e); t
    | EObjCast (_, t, e) -> ignore (typ_of_value_init e); t
    | ECheat (_, t, e) -> ignore (typ_of_value_init e); t
    | _ -> typ_of_value exp

let cmp_props (k1, _) (k2, _) = match String.compare k1 k2 with
  | 0 -> raise (Not_wf_typ ("the field " ^ k1 ^ " is repeated"))
  | n -> n

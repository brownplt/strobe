open Prelude
open Typedjs_syntax

exception Not_value of string

let tc_const (const : JavaScript_syntax.const) = match const with
  | JavaScript_syntax.CString s -> TRegex (P.singleton s)
  | JavaScript_syntax.CRegexp _ -> TId "RegExp"
  | JavaScript_syntax.CNum _ -> TPrim "Num"
  | JavaScript_syntax.CInt _ -> TPrim "Num"
  | JavaScript_syntax.CBool true -> TPrim "True"
  | JavaScript_syntax.CBool false -> TPrim "False"
  | JavaScript_syntax.CNull -> TPrim "Null"
  | JavaScript_syntax.CUndefined -> TPrim "Undef"

let typ_of_value (exp : exp) : typ = 
  let mk_field f (name, exp) = (P.singleton name, Present, f exp) in
  let rec f e = match e with
    | EAssertTyp (_, t, EFunc _) -> t
    | EObject (_, fs) -> 
      (* TODO: Everything else should be absent *)
      TObject 
  (mk_obj_typ
     ((proto_pat, Present, TId "Object") :: 
         (map (mk_field f) fs))
     P.empty)
    | EConst (_, c) -> tc_const c
    | EFunc (p, _, _, _) -> 
      raise (Not_value
               (sprintf "unannotated function at %s" (string_of_position p)))
    | ERef (_, RefCell, e') -> TRef begin match e' with
        | EObject (_, fields) -> 
    (* TODO: as above *)
    TObject
      (mk_obj_typ
         ((proto_pat, Present, TId "Object") 
    :: (map (mk_field f) fields)) P.empty)
        | EConst (_, c) -> tc_const c
        | EAssertTyp (_, t, EFunc _) -> t
        | EFunc (p, _, _, _) -> 
          raise (Not_value
                   (sprintf "unannotated function at %s"
                      (string_of_position p)))
        | _ -> raise (Not_value  (FormatExt.to_string Pretty.p_exp e'))
      end
    | _ -> raise (Not_value (FormatExt.to_string Pretty.p_exp e)) in
    f exp

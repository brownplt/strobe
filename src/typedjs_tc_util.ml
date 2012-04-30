open Prelude
open Typedjs_syntax
open TypImpl

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
               (sprintf "unannotated function at %s" (Pos.toString p)))
    | ERef (_, RefCell, e') -> TRef (None, begin match e' with
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
                    (Pos.toString p)))
      | _ -> raise (Not_value  (string_of_exp e'))
    end)
    | _ -> raise (Not_value (string_of_exp e)) in
  f exp

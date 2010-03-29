open Prelude
open Typedjs_syntax
open Typedjs_env
module X = Xml

let err key _ _ = 
  failwith (key ^ "is a repeated name in the XML")

let default_fields =
  [ ("innerText", TRef (TApp ("String", []))) ]

let rec t_xml (xml : X.xml) : typ IdMap.t = match xml with
  | X.PCData _ -> IdMap.empty
  | X.Element (name, att, children) -> begin try
      let name = List.assoc "name" att in
        (* TODO: remove name from fields *)
      let fields : (string * typ) list = 
        map (fun (x, _) -> (x, TRef (TApp ("String", [])))) att in
      let fields = default_fields @ fields in
      let child_maps  = map t_xml children in
        IdMap.add name (TRef (TObject fields))
          (fold_left (IdMapExt.join err) IdMap.empty child_maps)
    with Not_found ->
      fold_left (IdMapExt.join err) IdMap.empty (map t_xml children) 
    end

let parse_xml cin = 
  try X.parse_in cin
  with X.Error (msg, pos) ->
    eprintf "line %d, %s\n" (X.line pos) (X.error_msg msg);
    raise (X.Error (msg, pos))

      
let env_of_html cin = 
  let f x t env = Env.bind_id x t env in 
    IdMap.fold f (t_xml (parse_xml cin)) Env.empty_env

let env_of_strings cin = match parse_xml cin with
  | X.Element ("strings", _, children) ->
      let f env x = match x with
        | X.Element (str_id, _, _) -> 
            Env.bind_id str_id (TSource (TApp ("String", []))) env
        | _ -> env in
        fold_left f Env.empty_env children
  | _ -> 
      failwith "expected <strings>"

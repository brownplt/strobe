open Prelude

type runtime_type =
    RTNumber
  | RTString
  | RTBoolean
  | RTFunction
  | RTObject
  | RTUndefined

module RTOrdered = struct
  type t = runtime_type
  let compare = Pervasives.compare
end

module RTSet = Set.Make (RTOrdered)

module RTSetExt = SetExt.Make (RTSet)

type abs_value =
    AVType of RTSet.t
  | AVTypeof of id
  | AVString of string
  | AVTypeIs of id * RTSet.t

type env = abs_value IdMap.t

let empty_env = IdMap.empty

let single t = AVType (RTSet.singleton t)

let rec union_abs_value v1 v2 = match v1, v2 with
    AVType s1, AVType s2 ->
      AVType (RTSet.union s1 s2)
  | AVTypeof x, AVTypeof y -> 
      if x = y then AVTypeof x 
      else AVType (RTSet.singleton RTString)
  | AVString x, AVString y ->
      if x = y then AVString x
      else AVType (RTSet.singleton RTString)
  | AVTypeIs (x1, t1), AVTypeIs (x2, t2) -> 
      if x1 = x2 then AVTypeIs (x1, RTSet.union t1 t2)
      else AVType (RTSet.singleton RTBoolean)
  (* remaining cases move specialized values up the lattice *)
  | AVTypeIs _, v2 -> union_abs_value (single RTBoolean) v2
  | v1, AVTypeIs _ -> union_abs_value v1 (single RTBoolean)
  | AVTypeof _, v2 -> union_abs_value (single RTString) v2
  | v1, AVTypeof _ -> union_abs_value v1 (single RTString)
  | AVString _, v2 -> union_abs_value (single RTString) v2
  | v1, AVString _ -> union_abs_value v1 (single RTString)

let union_env env1 env2 = IdMapExt.join union_abs_value env1 env2

let lookup_env x env =
  try IdMap.find x env
  with Not_found -> AVType RTSet.empty

let bind_env x v env = 
  let f v = match v with
      AVTypeof x' when x = x' -> AVType (RTSet.singleton RTString)
    | _ -> v
  in IdMap.add x v (IdMap.map f env)



open Format

let pretty_runtime_type (fmt : formatter) (rt : runtime_type) = match rt with
    RTNumber -> pp_print_string fmt "number"
  | RTString -> pp_print_string fmt "string"
  | RTBoolean -> pp_print_string fmt "boolean"
  | RTObject -> pp_print_string fmt "object"
  | RTUndefined -> pp_print_string fmt "undefined"
  | RTFunction -> pp_print_string fmt "function"

let pretty_abs_value (fmt : formatter) (v : abs_value) = match v with
    AVType s -> RTSetExt.pretty fmt pretty_runtime_type s
  | AVTypeof x -> fprintf fmt "typeof %s" x
  | AVString s -> pp_print_string fmt ("\"" ^ s ^ "\"")
  | AVTypeIs (x, s) -> 
      fprintf fmt "typeof %s === " x;
      RTSetExt.pretty fmt pretty_runtime_type s

let pretty_env (fmt : formatter) (env : env) = 
  let pr x av =
    fprintf fmt "%s = " x;
    pretty_abs_value fmt av;
    pp_print_newline fmt ()
  in IdMap.iter pr env
    



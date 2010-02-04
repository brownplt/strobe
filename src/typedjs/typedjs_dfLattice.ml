open Prelude
open Typedjs_syntax

let any_runtime_typ =
  RTSetExt.from_list [ RTNumber; RTString; RTBoolean; RTFunction; RTObject; 
                       RTUndefined ]

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


let env_binds x env = IdMap.mem x env

let abs_value_to_runtime_typs (v : abs_value) : runtime_typs = match v with
    AVType rts -> rts
  | AVTypeof _ -> RTSet.singleton RTString
  | AVString _ -> RTSet.singleton RTString
  | AVTypeIs _ -> RTSet.singleton RTBoolean


let pretty_env (fmt : Format.formatter) (env : env) = 
  let pr x av =
    Format.fprintf fmt "%s = " x;
    Typedjs_pretty.pretty_abs_value fmt av;
    Format.pp_print_newline fmt ()
  in IdMap.iter pr env

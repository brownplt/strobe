open Prelude
open Typedjs_fromExpr
open JavaScript_syntax
open Typedjs_syntax
open JavaScript

module Int = struct
  type t = int
  let compare = compare
end

module IntMap = Map.Make (Int)
module IntMapExt = MapExt.Make (Int) (IntMap)


let is_before stop_ix (start_ix, _) =
  start_ix < stop_ix


let transform_exprs fs cin cout : unit =
  let curr_ix = ref 0 in

  let rec apply lst end_ix = match lst with
    | [] -> output cout cin !curr_ix (end_ix - !curr_ix)
    | (start_ix, (stop_ix, prefix, suffix)) :: rest ->
        output_string cout (String.sub cin !curr_ix (start_ix - !curr_ix));
        let expr_buf = String.sub cin start_ix (stop_ix - start_ix) in
          ignore (parse_expr expr_buf "<expr_transformer.ml>"); (* sanity *)
          output_string cout prefix;
          let nested, next = take_while (is_before stop_ix) rest in
            curr_ix := start_ix;
            apply nested stop_ix;
            output_string cout suffix;
            curr_ix := stop_ix;
            apply next end_ix in
    apply (List.rev (IntMapExt.to_list fs)) (String.length cin)

type contract =
  | CPred of string * expr
  | CArrow of contract list * contract

let p = (Lexing.dummy_pos, Lexing.dummy_pos)

let contract_lib = VarExpr (p, "contracts")
  
let flat p name =  CPred (name, DotExpr (p, contract_lib, name))
  
let rec ctc_of_typ p (typ : typ) = match typ with
  | TArrow (_, args, result) -> 
      CArrow (map (ctc_of_typ p) args, ctc_of_typ p result)
  | TConstr ("Int", []) -> flat p "Int"
  | TConstr ("Str", []) -> flat p "Str"
  | TConstr ("Undefined", []) -> flat p "Undefined"
  | TConstr ("NotUndef", []) -> flat p "NotUndefined"
  | _ -> 
      failwith (sprintf "cannot create a contract for the type %s"
                  (FormatExt.to_string Typedjs_syntax.Pretty.p_typ typ))

let rec cc p (ctc : contract) : expr = match ctc with
  | CPred (name, expr) -> 
      (* predicates are already wrapped *)
      expr 
  | CArrow (args, result) ->
      CallExpr
        (p, CallExpr (p, DotExpr (p, contract_lib, "varArityFunc"),
                      [ ConstExpr (p, CString "function") ]),
         [ ArrayExpr (p, map (cc p) args); 
           ArrayExpr (p, []); (* zero var-arity arguments *)
           cc p result ])

let pp expr = FormatExt.to_string Pretty.p_expr expr

let mk_contract_transformers typs =
  let f (stop_ix, typ) =
    let e' = cc p (ctc_of_typ p typ) in
    let prefix = (pp contract_lib) ^ ".guard(" ^ pp e' ^ ", " in
    let suffix = 
      ", \"callee\", \"caller\", \"" ^ string_of_position p ^ "\")" in
      (stop_ix, prefix, suffix) in
    IntMap.map f typs
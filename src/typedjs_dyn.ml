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


let transform_exprs fs cin cout : unit =
  let curr_ix = ref 0 in
  let apply start_ix (end_ix, f) = 
    output_string cout (String.sub cin !curr_ix (start_ix - !curr_ix));
      let expr_buf = String.sub cin start_ix (end_ix - start_ix) in
        (* A sanity check *)
        let expr = parse_expr expr_buf "<expr_transformer.ml>" in
        let expr = f expr in
        let expr_str = FormatExt.to_string Pretty.p_expr expr in
          output_string cout expr_str;
          curr_ix := end_ix in
    IntMap.iter apply fs;
    output cout cin !curr_ix (String.length cin - !curr_ix)

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
  | TConstr ("String", []) -> flat p "String"
  | TConstr ("Undefined", []) -> flat p "Undefined"
  | _ -> raise (Invalid_argument "ctc_of_typ")

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

let mk_contract_transformers typs =
  let f typ =
    let e' = cc p (ctc_of_typ p typ) in
      fun expr ->
        CallExpr (p, DotExpr (p, contract_lib, "guard"),
                  [ e'; expr;
                    ConstExpr (p, CString "callee");
                    ConstExpr (p, CString "caller");
                    ConstExpr (p, CString (string_of_position p)) ]) in
    IntMap.map (second2 f) typs

open Prelude
open Contract_compiler
open Typedjs_syntax
open JavaScript_syntax

exception Typ_to_contract of pos * string

module S = struct

  open Typedjs_fromExpr

  let contract_lib = VarExpr ((Lexing.dummy_pos, Lexing.dummy_pos), "contracts")

  let flat p name =  CPred (name, DotExpr (p, contract_lib, name))
    
  let rec ctc_of_typ p (typ : typ) = match typ with
    | TArrow (_, args, result) -> 
        CArrow (map (ctc_of_typ p) args, ctc_of_typ p result)
    | TApp ("Int", []) -> flat p "Int"
    | TApp ("String", []) -> flat p "String"
    | TApp ("Undefined", []) -> flat p "Undefined"
    | _ -> raise (Invalid_argument "ctc_of_typ")
    
  let contract_parser p text = match parse_annotation p text with
    | ADowncast typ -> begin
        try Some (ctc_of_typ p typ)
        with Invalid_argument "ctc_of_typ" ->
          raise (Typ_to_contract
                   (p, "cannot convert this type to a contract: " ^ text))
      end
    | _ -> None

end

module M = Make (S)

let types_to_contracts = M.insert_contracts




     

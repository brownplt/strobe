open Prelude
open Typedjs_fromExpr
open JavaScript_syntax
open Typedjs_syntax
open JavaScript
open Typedjs_dyn_supp

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
          ignore (parse_expr expr_buf "<expr_transformer.ml>");
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
  | CUnion of contract * contract
  | CObject of (pat * contract) list

let p = (Lexing.dummy_pos, Lexing.dummy_pos)

let contract_lib = VarExpr (p, "contracts")
  
let flat p name =  CPred (name, DotExpr (p, contract_lib, name))

let instanceof p name =  
  CPred (name, CallExpr (p, DotExpr (p, contract_lib, "Instanceof"),
                         [ VarExpr (p, name) ]))

let rec ctc_of_typ p (typ : typ) = match typ with
  | TArrow (args, result) -> 
      CArrow (map (ctc_of_typ p) args, ctc_of_typ p result)
  | TPrim (Int) -> flat p "Int"
(*  | TPrim (Str) -> flat p "Str" *)
  | TPrim (Undef) -> flat p "Undef"
  | TPrim (True) 
  | TPrim (False) -> flat p "Bool"
  | TUnion (s, t) -> CUnion (ctc_of_typ p s, ctc_of_typ p t)
  | TObject o ->
      let typ_of_prop p = begin match p with
        | PPresent typ
        | PMaybe typ -> typ
        | PAbsent -> failwith "Cannot make contract for absent field"
      end in
      CObject (map (fun (f, pro) -> (f, ctc_of_typ p (typ_of_prop pro))) 
		 (fields o))
  | TRef t -> ctc_of_typ p t (* \JS artifacts, source and sinks are moot since
                                contracts don't preserve identity *)
  | TSource t -> ctc_of_typ p t 
  | TSink t -> ctc_of_typ p t
  | _ -> 
      failwith (sprintf "cannot create a contract for the type %s"
		  (string_of_typ typ))

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
  | CObject fields ->
      failwith "Broken by regex fields"
  | CUnion (lhs, rhs) ->
      CallExpr
        (p, CallExpr (p, DotExpr (p, contract_lib, "union"),
                      [ ConstExpr (p, CString "union") ]),
         [ cc p lhs; cc p rhs ])

let pp expr = FormatExt.to_string Pretty.p_expr expr

let mk_contract_transformers typs =
  let f (stop_ix, typ) =
    let e' = cc p (ctc_of_typ p typ) in
    let prefix = (pp contract_lib) ^ ".guard(" ^ pp e' ^ ", " in
    let suffix = 
      ", \"callee\", \"caller\", \"" ^ string_of_position p ^ "\")" in
      (stop_ix, prefix, suffix) in
    IntMap.map f typs



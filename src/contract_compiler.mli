open Prelude
open JavaScript_syntax

type contract =
  | CPred of string * expr
  | CArrow of contract list * contract

module type Contracts = sig

  val contract_lib : expr

  val contract_parser : pos -> string -> contract option

end

module Make : functor (C : Contracts) -> sig
  val insert_contracts : prog -> prog
end

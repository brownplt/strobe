open Prelude

let instanceof_contracts : IdSet.t ref = ref IdSet.empty

let assume_instanceof_contract name =
  instanceof_contracts := IdSet.add name !instanceof_contracts

let is_instanceof_contract name = IdSet.mem name !instanceof_contracts

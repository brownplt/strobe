open Prelude
open Typedjs_syntax
open JavaScript_syntax

module IntMap : Map.S
  with type key = int

module IntMapExt : MapExt.S
  with type key = int
  with type +'a t = 'a IntMap.t

val transform_exprs : (int * (expr -> expr)) IntMap.t -> string
  -> out_channel -> unit

val mk_contract_transformers : (int * typ) IntMap.t 
  -> (int * (expr -> expr)) IntMap.t

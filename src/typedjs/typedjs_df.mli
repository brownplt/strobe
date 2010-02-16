(** Local type analysis for Typed JavaScript expressions in A-normal form. *)

open Prelude
open Typedjs_anf
open Typedjs_dfLattice
open Typedjs_syntax

type bound_id = id * pos

module BoundIdMap : Map.S
  with type key = bound_id


(** [local_type_analysis env anfexp] returns a map from bound
    identifiers to abstract values. The analysis is intraprocedural, so
    the map does not bind nodes from nested functions. *)
val local_type_analysis : env -> anfexp -> abs_value BoundIdMap.t

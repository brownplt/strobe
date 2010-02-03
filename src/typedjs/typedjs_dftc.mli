open Prelude
open Typedjs_syntax

val runtime : typ -> abs_value

val static : runtime_typs -> typ -> typ

(** [annotate env available_ids exp = annotated_exp, still_available_ids] 
    returns [annotated_exp], which is [exp] with dataflow annotations.
    [exp] should be the body of a function. [available_ids] is the set of
    variables that may receive dataflow annotations. (A variable may receive
    dataflow annotations in just one function.)

    [annotate] also returns [still_available_ids], which is a subset of of
    [available_ids]. This set should be threaded between functions in the
    same block. *)
val annotate : typ IdMap.t -> IdSet.t -> pos exp -> pos exp


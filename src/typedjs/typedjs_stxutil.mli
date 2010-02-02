(** Various utilities for working with Typed JavaScript syntax. *)
open Prelude
open Typedjs_syntax

(** [av_exp e] returns the free variables that are assigned to in [e]. An 
    expression of the form [EAssign (_, LVar _, _)] is an assignment. *)
val av_exp : 'a exp -> IdSet.t

(** [local_av_exp e] is similar to [av_exp], but does not recur into nested
    functions: it returns the locally-assigned free variables of [e]. *)
val local_av_exp : 'a exp -> IdSet.t

(** [nested_funcs e] returns a list of [EFunc _] expressions nested within [e].
    If [e] is itself an [EFunc _], it returns [e]. *)
val nested_funcs : 'a exp -> 'a exp list

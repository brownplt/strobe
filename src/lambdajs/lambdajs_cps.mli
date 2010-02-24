(** The datatypes and algorithms in this module are based on Chapters 2 and 5
    of "Compiling with Continuations" by Andrew W. Appel. We depart from CWC
    in the following ways:

    - We do not use a global variable for the current exception handler.
    Instead, all functions receive an additional exception continuation.
    CWC discusses a pitfall with this technique. Note that JavaScript's 
    operators almost never signal exceptions. If a LambdaJS operator signals
    an exception, it indicates a bug in desugaring.

    - LambdaJS has try-finally and break expressions that do not correspond to
    anything in the lambda language of CWC.


*)

open Prelude
open Lambdajs_syntax

type cpsval =
    Const of Exprjs_syntax.const
  | Array of cpsval list
  | Object of (string * cpsval) list
  | Id of id

type node = int * pos

type cpsexp =
    Fix of node * (id * id list * cpsexp) list * cpsexp
  | App of node * cpsval * cpsval list
  | If of node * cpsval * cpsexp * cpsexp
  | Let0 of node * id * cpsval * cpsexp (* load immediate / reg-reg move *)
  | Let1 of node * id * op1 * cpsval * cpsexp
  | Let2 of node * id * op2 * cpsval * cpsval * cpsexp
  | GetField of node * id * cpsval * cpsval * cpsexp
  | DeleteField of node * id * cpsval * cpsval * cpsexp
  | UpdateField of node * id * cpsval * cpsval * cpsval * cpsexp
  | Ref of node * id * cpsval * cpsexp
  | SetRef of node * cpsval * cpsval * cpsexp
  | Deref of node * id * cpsval * cpsexp

val cps : exp -> cpsexp

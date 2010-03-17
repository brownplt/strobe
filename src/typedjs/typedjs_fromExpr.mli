(** In this module, we transform Expr{_ JS} into Typed
    JavaScript by enforcing Typed JavaScript's syntactic restrictions
    and parsing type annotations.  In addition, we perform some
    transformations to make type-checking simpler.

    The major restrictions and transformations are as follows:

    - We remove scope objects, transforming them to let-bindings. A
    [VarDeclExpr] at the head of a [SeqExpr] is treated as a let-binding
    whose body is the tail of the same [SeqExpr].  For example, the
    following JavaScript statement:
 {[ \{
  var x = 24;
  return x + 2; 
\} ]}
    is transformed to:
{[
(let [x 23]
  (break "%return" (+ x 2)))
]}
    This transformation disallows certain legitimate (but poorly
    written) JavaScript patterns. For example, the following error-free program:
{[
\{
  \{
    var x = 24;
    x + 43;
  \}
  x + 50
\} ]}
    is transformed to:
{[
(seq
  (let [x 24]
    (+ x 43))
  (+ x 50)) ]}
    where the last [x] is unbound. If we do this transformation naively, the
    unbound [x] may be captured by an enclosing binding. For example, this
program:
{[function(x)  \{
  \{
    var x = 24;
    x + 43;
  \}
  x + 50;
\} ]}
should {i not} be transformed to this expression:
{[
(lambda (x)
  (seq
    (let [x 24]
      (+ x 43))
    (+ x 50))) ]}
In this case, we report that [x] is unbound in [(+ x 50)].

    - We ensure that variables are defined before they are used.

    - We transform function statements to function expressions. We do
    not need to lift function statements, since Typed JavaScript's
    lexical scoping will prevent lifted functions from being used before
    they are defined in source. (Here, we use "lift" as it used in the
    JavaScript specification.)

    - We remove loops, transforming them to fixpoints.

    - We ensure that each function's argument list does not contain
    duplicate argument names.

    - We ensure that each object literal's field list does not contain
    duplicate field definitions.

    - We ensure that functions have type annotations. We also require
    the annotation to be an arrow type. This restriction makes
    type-checking simpler, as the type of the ["%return"] label is the
    return type of the function.

    - We process [/*: mutable */] annotations on object literals' fields.



*)
open Typedjs_syntax
open Prelude
open JavaScript_syntax
open Typedjs_env

(** [from_exprjs] raises this exception. *)
exception Not_well_formed of pos * string

val init_types : (pos * string) list -> unit


(** @raise Not_well_formed if any of the well-formed criteria are
   violated (see above).
*)
val from_exprjs : Env.env -> Exprjs_syntax.expr -> def


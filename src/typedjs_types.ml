open Prelude
open Typedjs_syntax

(** Some of these constructor names correspond directly to the object names
    in JavaScript. Good or bad? *)

let typ_str = TConstr ("Str", [])

let typ_regexp = TConstr ("RegExp", [])

let typ_num = TConstr ("Num", [])

let typ_int = TConstr ("Int", [])

let typ_bool = TConstr ("Bool", [])

let typ_true = TConstr ("True", [])
let typ_false = TConstr ("False", [])

let typ_null = TConstr ("Null", [])

let typ_undef = TConstr ("Undef", [])

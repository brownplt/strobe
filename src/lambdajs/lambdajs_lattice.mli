open Prelude
open Lambdajs_cps

type absval

type absobj

type absenv

val empty_absval : absval

val abs_const : Exprjs_syntax.const -> absval

val abs_union : absval -> absval -> absval

val abs_ref : int -> absval

val locs_of_val : absval -> int list

val objs_of_val : absval -> absobj list

val strings_of_val : absval -> string list

val closures_of_val : absval -> (int * id list * cpsexp) list

val closure  : id list -> cpsexp -> absval

val upd_field : string -> absval -> absobj -> absobj

val get_field : string -> absobj -> absval

val val_of_obj : absobj -> absval

val mk_absobj : (string * absval) list -> absobj

val any_num : absval

val any_str : absval

val any_bool : absval

val abs_undef : absval

(* val p_av : absval -> FormatExt.printer *)

val is_any_str : absval -> bool

val union_env : absenv -> absenv -> absenv

val lookup : id -> absenv -> absval

val bind : id -> absval -> absenv -> absenv

val empty_absenv : absenv

val is_true : absval -> bool

val is_false : absval -> bool

val eq_absenv : absenv -> absenv -> bool

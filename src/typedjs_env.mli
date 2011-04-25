open Prelude
open Typedjs_syntax

exception Not_wf_typ of string

type subtype_exn =
  | ExtraFld of pat * prop
  | MismatchTyp of typ * typ
  | MismatchFld of (pat * prop) * (pat * prop)

exception Not_subtype of subtype_exn

module Env : sig
  
  type env

  val empty_env : env

  val bind_id : id -> typ -> env -> env

  val bind_lbl : id -> typ -> env -> env

  val bind_typ_id : id -> typ -> env -> env

  val lookup_id : id -> env -> typ

  val lookup_lbl : id -> env -> typ

  (** JavaScript cannot perform a labelled jump across a function. *)
  val clear_labels : env -> env

  val id_env : env -> typ IdMap.t

  val dom : env -> IdSet.t

  (** [set_global_object env class_name] adds all the fields of [class_name]
      to the environment. *)
  val set_global_object : env -> string -> env

  (** [subtype end typ1 typ2] assumes that [typ1] and [typ2] are in normal form.
      The [env] is needed for bounded quantification. *)
  val subtype : env -> typ -> typ -> bool

  val fields : pos -> env -> typ -> Sb_strPat.t -> typ

  val assert_subtyp : env -> pos -> typ -> typ -> unit

  (** [subtypes typs1 typs2] applies [subtype] pairwise to the elements of
      [typs1] and [typs2]. If the lists have unequal lengths, it returns
      [false]. *)
  val subtypes : env -> typ list -> typ list -> bool

  val typ_union : env -> typ -> typ -> typ

  val typ_intersect : env -> typ -> typ -> typ

  val static : env -> RTSet.t -> typ -> typ

  val bind_typ : env -> typ -> env * typ

end


val parse_env : in_channel -> string -> env_decl list

val extend_global_env : Env.env -> env_decl list -> Env.env

(** [typ_subst x s t] is capture-free substitution of the type variable [x]
    for the type [s] in the type [t]. *)
val typ_subst : id -> typ -> typ -> typ

val simpl_typ : Env.env -> typ -> typ

val typ_assoc : Env.env -> typ -> typ -> typ IdMap.t

val fields : pos -> Env.env -> typ -> Sb_strPat.t -> typ

val typid_env : Env.env -> typ IdMap.t

(*
val operator_env_of_tc_env : Env.env 
  -> (Typedjs_lattice.av list -> Typedjs_lattice.av) IdMap.t
*)

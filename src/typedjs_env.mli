open Prelude
open Typedjs_syntax

module Env : sig
  
  type env

  val empty_env : env

  val bind_id : id -> typ -> env -> env

  val bind_lbl : id -> typ -> env -> env

  val bind_typ_id : id -> typ -> env -> env

  val lookup_id : id -> env -> typ

  val lookup_lbl : id -> env -> typ

  val field_typ : env -> constr -> id -> typ option

  val is_class : env -> id -> bool

  (** JavaScript cannot perform a labelled jump across a function. *)
  val clear_labels : env -> env

  val id_env : env -> typ IdMap.t

  val dom : env -> IdSet.t

  (** A new class with no methods. *)
  val new_root_class : env -> id -> env

  (** Adds a method to a class. *)
  val add_method : id -> id -> typ -> env -> env

  (** [set_global_object env class_name] adds all the fields of [class_name]
      to the environment. *)
  val set_global_object : env -> string -> env

  val check_typ : pos -> env -> typ -> typ

  (** [subtype typ1 typ2] assumes that [typ1] and [typ2] are in normal form. *)
  val subtype : env -> typ -> typ -> bool

  (** [subtypes typs1 typs2] applies [subtype] pairwise to the elements of
      [typs1] and [typs2]. If the lists have unequal lengths, it returns
      [false]. *)
  val subtypes : env -> typ list -> typ list -> bool

  val typ_union : env -> typ -> typ -> typ

  val static : env -> RTSet.t -> typ -> typ

  val bind_typ : env -> typ -> env * typ

end


val parse_env : in_channel -> string -> env_decl list

val extend_global_env : Env.env -> env_decl list -> Env.env

val cf_env_of_tc_env : Env.env -> Typedjs_lattice.env

(** [typ_subst x s t] is capture-free substitution of the type variable [x]
    for the type [s] in the type [t]. *)
val typ_subst : id -> typ -> typ -> typ

val unify_typ : typ -> typ -> typ IdMap.t

val operator_env_of_tc_env : Env.env 
  -> (Typedjs_lattice.av list -> Typedjs_lattice.av) IdMap.t

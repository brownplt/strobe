open Prelude
open Typedjs_syntax

module Env : sig
  
  type env

  val empty_env : env

  val bind_id : id -> typ -> env -> env

  val bind_lbl : id -> typ -> env -> env

  val lookup_id : id -> env -> typ

  val lookup_lbl : id -> env -> typ

  val lookup_class : id -> env -> typ

  val get_classes : env -> typ IdMap.t (* ocaml sucks *)

  (** JavaScript cannot perform a labelled jump across a function. *)
  val clear_labels : env -> env

  val id_env : env -> typ IdMap.t

  val dom : env -> IdSet.t

  (** A new class with no methods. *)
  val new_class : id -> env -> env


  (** Adds a method to a class. *)
  val add_method : id -> id -> typ -> env -> env

 (** Used to load environments from multiple files. *)
  val union : env -> env -> env

  (** [set_global_object env class_name] adds all the fields of [class_name]
      to the environment. *)
  val set_global_object : env -> string -> env

  val check_typ : pos -> env -> typ -> typ

end


val parse_env : in_channel -> string -> env_decl list

val mk_env : env_decl list -> Env.env

val cf_env_of_tc_env : Env.env -> Typedjs_lattice.env

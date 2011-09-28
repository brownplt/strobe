open Prelude
open Typedjs_syntax

exception Not_wf_typ of string


  type env

  val empty_env : env

  val kind_check : env -> typ -> kind

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

  (** [subtypes typs1 typs2] applies [subtype] pairwise to the elements of
      [typs1] and [typs2]. If the lists have unequal lengths, it returns
      [false]. *)
  val subtypes : env -> typ list -> typ list -> bool

  val typ_union : env -> typ -> typ -> typ

  val typ_intersect : env -> typ -> typ -> typ

  val static : env -> RTSet.t -> typ -> typ

  val bind_typ : env -> typ -> env * typ


val parse_env : in_channel -> string -> env_decl list

val extend_global_env : env -> env_decl list -> env

val simpl_typ : env -> typ -> typ

val expose : env -> typ -> typ

val typ_assoc : env -> typ -> typ -> typ IdMap.t

val inherits : pos -> env -> typ -> P.t -> typ

val tid_env : env -> (typ * kind) IdMap.t

val typid_env : env -> typ IdMap.t

val extend_env : typ IdMap.t -> (typ * kind) IdMap.t -> env -> env

val verify_env : env -> unit

open Prelude
open Typedjs_syntax

module type EnvType = sig
  
  type env

  val empty_env : env

  val bind_id : id -> typ -> env -> env

  val bind_lbl : id -> typ -> env -> env

  val lookup_id : id -> env -> typ

  val lookup_lbl : id -> env -> typ

  val assignable_ids : env -> IdSet.t

  val new_assignable_id : id -> env -> env

  val remove_assigned_ids : IdSet.t -> env -> env

end

module Env : EnvType

val tc_exp : Env.env -> pos exp -> typ


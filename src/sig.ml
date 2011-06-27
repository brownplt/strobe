open Prelude

module type EQ = sig

  type t

  val is_equal : t -> t -> bool

end

module type PRINTABLE = sig
  type t
  val pretty : t -> string
end

module type PAT = sig
    
  (** string patterns *)
  type t

  val parse : Lexing.position -> string -> t
    
  val singleton : string -> t
  val singleton_string : t -> string option
    
  val empty : t
  val all : t
    
  val intersect : t -> t -> t
  val union : t -> t -> t
  val negate : t -> t
  val subtract : t -> t -> t
  val concat : t -> t -> t
    
  val is_empty : t -> bool

  val is_overlapped : t -> t -> bool
    
  (** [is_subset pat1 pat2] is true if all strings in [pat1] are also in 
      [pat2]. *)
  val is_subset : t -> t -> bool

  val is_equal : t -> t -> bool

  (** [example pat] returns an example of a string in [pat]. *)
  val example : t -> string option

  val pretty : t -> string
end

module type SET = sig

  (** Type of sets *)
  type repr

  (** Set constructors and external representation *)
  type t = 
    | Pat of repr
    | Var of Id.t
    | Union of t * t
    | Inter of t * t
    | Diff of t * t
    | Empty
    | All

  val simpl : t -> t

  val pretty : t -> string

  val is_empty : t -> bool
  val is_overlapped : t -> t -> bool
  val is_subset : t -> t -> bool
  val is_equal : t -> t -> bool

  (** [example pat] returns an example of a string in [pat]. *)
  val example : t -> string option

end

module type PATV = sig

  module P : PAT

  type t = 
    | Pat of P.t 
    | Var of Id.t
    | Union of t * t
    | Inter of t * t
    | Diff of t * t

  val is_equal : t -> t -> bool (* always signals an exception *)

end

module type SAT = sig

  module EQ : EQ

  type t =
    | And of t * t
    | Or of t * t
    | Not of t
    | Var of Id.t
    | True
    | False
    | Imp of t * t
    | Eq of EQ.t * EQ.t

  val is_sat : t -> bool

  val cnf : t -> t
  val dnf : t -> t

  val undisjunct : t -> t list
  val unconjunct : t -> t list

end

module type MLS = sig

  module PAT : PAT
  module PATV: PATV with module P = PAT
  module SAT : SAT with module EQ = PATV
  module SimplSAT : SAT with module EQ = PAT

  val is_sat : SAT.t -> bool

end

module type TYP = sig

  exception Typ_error of pos * string
  exception Not_subtype of string

(*  module StrLang : PAT
  module Pat : PATV with module P = StrLang *)

  type pat


  type prim =
    | Num
    | Int
    | True
    | False
    | Undef
    | Null

  type kind = 
    | KStar
    | KArrow of kind list * kind
	
  type typ = 
    | TPrim of prim
    | TUnion of typ * typ
    | TIntersect of typ * typ
    | TArrow of typ list * typ
    | TObject of obj_typ
    | TRegex of pat
    | TRef of typ
    | TSource of typ
    | TSink of typ
    | TTop
    | TBot
    | TForall of id * typ * typ (** [TForall (a, s, t)] forall a <: s . t *)
    | TId of id
    | TRec of id * typ 
    | TLambda of (id * kind) list * typ (** type operator *)
    | TApp of typ * typ list (** type operator application *)
    | TFix of id * kind * typ (** recursive type operators *)

  and obj_typ
      
  and prop = 
    | PInherited of typ
    | PPresent of typ
    | PMaybe of typ
    | PAbsent

  type field = pat * prop 

  type typenv = (typ * kind) IdMap.t

  val string_of_typ : typ -> string
  val string_of_kind : kind -> string

  val proto_str : string

  val proto_pat : pat

  val mk_obj_typ : field list -> obj_typ

  val fields : obj_typ -> field list

  val typ_subst : id -> typ -> typ -> typ

  val simpl_typ : typenv -> typ -> typ

  val expose : typenv -> typ -> typ

  val inherits : typenv -> typ -> pat -> typ

  val typ_union : typenv -> typ -> typ -> typ

  val typ_intersect : typenv -> typ -> typ -> typ

  val subtypes : typenv -> typ list -> typ list -> bool

  val subtype : typenv -> typ -> typ -> bool

  val assert_subtyp : typenv -> pos -> typ -> typ -> unit

end

(** Types written by users. *)
module type WRITTYP = sig

  module Pat : PAT
  module Typ : TYP with type pat = Pat.t

  type t = 
    | Str
    | Bool
    | Prim of Typ.prim
    | Union of t * t
    | Inter of t * t
    | Arrow of t option * t list * t (** [Arrow (this, args, result)] *)
    | Object of f list
    | Pat of Pat.t
    | Ref of t
    | Source of t
    | Top
    | Bot
    | Id of id
    | Forall of id * t * t
    | Rec of id * t
    | Syn of id
    | Lambda of (id * Typ.kind) list * t
    | Fix of id * Typ.kind * t
    | App of t * t list
       
  and f = 
    | Present of Pat.t * t
    | Maybe of Pat.t * t
    | Inherited of Pat.t * t
    | Absent of Pat.t
    | Skull of Pat.t
    | Star of t option

  type env_decl =
    | EnvBind of pos * id * t
    | EnvType of pos * id * t

  type annotation =
    | ATyp of t
    | AUpcast of t
    | ADowncast of t
    | ATypAbs of id * t
    | ATypApp of t
    | AAssertTyp of t
    | ACheat of t

end

open Prelude

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
  val is_finite : t -> bool
  val is_overlapped : t -> t -> bool
    
  (** [is_subset pat1 pat2] is true if all strings in [pat1] are also in 
      [pat2]. *)
  val is_subset : t -> t -> bool

  val is_member : string -> t -> bool

  val is_equal : t -> t -> bool

  (** [example pat] returns an example of a string in [pat]. *)
  val example : t -> string option

  val pretty : t -> string
end

module type TYP = sig

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

  val proto_str : string

  val proto_pat : pat

  val mk_obj_typ : field list -> obj_typ

  val fields : obj_typ -> field list

  val typ_subst : id -> typ -> typ -> typ

  val simpl_typ : typenv -> typ -> typ

  val expose : typenv -> typ -> typ

  (** Returns the type of the parent of an object type. *)
  val parent_typ : typenv -> typ -> typ option

  val inherit_guard_pat : typ -> pat

end

module type TYPMake = functor (P : PAT) ->  (TYP with type pat = P.t)


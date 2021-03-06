open Prelude

module type EQ = sig

  type t

  val is_equal : t -> t -> bool

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
  val intersections : t list -> t
  val union : t -> t -> t
  val unions : t list -> t
  val negate : t -> t
  val subtract : t -> t -> t
  val concat : t -> t -> t

    
  val is_empty : t -> bool

  val is_overlapped : t -> t -> bool
    
  (** [is_subset pat1 pat2] is true if all strings in [pat1] are also in 
      [pat2]. *)
  val is_subset :t -> t -> bool

  val is_equal : t -> t -> bool

  (** [example pat] returns an example of a string in [pat]. *)
  val example : t -> string option

  val pretty : t -> string
end

module type SET = sig

  (** Type of sets *)
  type t

  val empty : t
  val all : t
    
  val intersect : t -> t -> t
  val intersections : t list -> t
  val union : t -> t -> t
  val unions : t list -> t
  val negate : t -> t
  val subtract : t -> t -> t
  val singleton : string -> t
  val singleton_string : t -> string option
  val var : string -> t


  val pretty : t -> string

  val is_empty : t -> bool
  val is_overlapped : t -> t -> bool
  val is_subset : t IdMap.t -> t -> t -> bool
  val is_equal : t -> t -> bool

  (** [example pat] returns an example of a string in [pat]. *)
  val example : t -> string option

  val parse : Lexing.position -> string -> t

end

module type TYP = sig
    
  module Pat : SET

  type pat = Pat.t

  type kind = 
    | KStar
    | KArrow of kind list * kind

  type presence = 
    | Inherited
    | Present
    | Maybe
  
  type typ = 
    | TPrim of string
    | TUnion of string option * typ * typ
    | TIntersect of string option * typ * typ
    | TArrow of typ list * typ option * typ (* args (including <this>), optional variadic arg, return typ *)
    | TThis of typ
    | TObject of obj_typ
    | TWith of typ * obj_typ
    | TRegex of pat
    | TRef of string option * typ
    | TSource of string option * typ
    | TSink of string option * typ
    | TTop
    | TBot
    | TForall of string option * id * typ * typ (** [TForall (a, s, t)] forall a <: s . t *)
    | TId of id
    | TRec of string option * id * typ 
    | TLambda of string option * (id * kind) list * typ (** type operator *)
    | TApp of typ * typ list (** type operator application *)
    | TFix of string option * id * kind * typ (** recursive type operators *)
    | TUninit of typ option ref (** type of uninitialized variables *)

  and obj_typ

  type typ_error_details =
    | TypKind of (typ -> kind -> string) * typ * kind
    | StringTyp of (string -> typ -> string) * string * typ
    | FixedString of string
    | String of (string -> string) * string
    | TypTyp of (typ -> typ -> string) * typ * typ
    | NumNum of (int -> int -> string) * int * int
    | Typ of (typ -> string) * typ
    | Pat of (pat -> string) * pat
    | PatPat of (pat -> pat -> string) * pat * pat
    | PatPatTyp of (pat -> pat -> typ -> string) * pat * pat * typ
    | PatTyp of (pat -> typ -> string) * pat * typ
    | TypTypTyp of (typ -> typ -> typ -> string) * typ * typ * typ


  exception Typ_error of Pos.t * typ_error_details
  exception Not_subtype of typ_error_details

  val typ_error_details_to_string : typ_error_details -> string

      
  type field = pat * presence * typ

  type typenv = (typ * kind) IdMap.t

  module Pretty : sig
    val typ : typ -> FormatExt.printer
    val kind : kind -> FormatExt.printer
    val useNames : bool ref
  end

  val string_of_typ : typ -> string
  val string_of_kind : kind -> string
  
  val expose_twith : typenv -> typ -> typ

  val proto_str : string

  val proto_pat : pat

  val mk_obj_typ : field list -> pat -> obj_typ

  val fields : obj_typ -> field list

  (** Pattern for absent field *)
  val absent_pat : obj_typ -> pat

  (** includes absent *)
  val cover_pat : obj_typ -> pat

  (** excludes absent *)
  val possible_field_cover_pat : obj_typ -> pat

  val merge : typ -> obj_typ -> typ

  val typ_subst : id -> typ -> typ -> typ

  val parent_typ : typenv -> typ -> typ option

  val simpl_typ : typenv -> typ -> typ

  val apply_name : string option -> typ -> typ

  val replace_name : string option -> typ -> typ

  val expose : typenv -> typ -> typ

  val simpl_lookup : Pos.t -> typenv -> typ -> pat -> typ

  val inherits : Pos.t -> typenv -> typ -> pat -> typ

  val typ_union : typenv -> typ -> typ -> typ

  val typ_intersect : typenv -> typ -> typ -> typ

  val subtypes : typenv -> typ list -> typ list -> bool

  val subtype : typenv -> typ -> typ -> bool

  val typ_mismatch : Pos.t -> typ_error_details -> unit

  val get_num_typ_errors : unit -> int

  val with_typ_exns : (unit -> 'a) -> 'a

  val pat_env : typenv -> pat IdMap.t

  (** [object_typs t] returns a list of object types in a union and a flag
      which is set if there were no other types in [t]. *)
  val object_typs : typ -> typ list * bool

end

open Prelude
open Lambdajs_cps

module ConstSet = Set.Make (Exprjs_syntax.Const)

(*
type absval = {
  absval_any_string : bool;
  absval_any_number : bool;
  absval_any_bool : bool;
  abs_consts : ConstSet.t;
  abs_closures : lambda IntMap.t; (* closures by body index *)
  abs_refs : IntSet.t; (* locations *)
  abs_objs : obj_set
}

and type obj_set = 
  | ObjSetEmpty
  | 
    *)

    
module rec AV : sig

  type t = 
    | ANumber
    | ABool
    | AString
    | AConst of Exprjs_syntax.const
    | ARef of int
    | AObj of avs IdMap.t
    | AArr of avs list
    | AClosure of int * id list * cpsexp
  and avs = AVSet.t
  and env = avs IdMap.t
  
  val compare : t -> t -> int
end 
= struct
  type t = 
    | ANumber
    | ABool
    | AString
    | AConst of Exprjs_syntax.const
    | ARef of int
    | AObj of avs IdMap.t
    | AArr of avs list
    | AClosure of int * id list * cpsexp
  and avs = AVSet.t
  and env = avs IdMap.t
      
  let compare = Pervasives.compare 

end
  
and AVSet 
  : Set.S with type elt = AV.t 
  = Set.Make (AV)

module AVSetExt = SetExt.Make (AVSet)

open AV

type absval = AV.avs

type absobj = absval IdMap.t

type absenv = absval IdMap.t

let empty_absval = AVSet.empty

let abs_union = AVSet.union

let abs_ref n = AVSet.singleton (ARef n)

let abs_const c = AVSet.singleton (AConst c)
let abs_undef = abs_const (Exprjs_syntax.CUndefined)

let locs_of_val av = 
  AVSet.fold (fun v lst -> match v with 
                | ARef loc -> loc :: lst
                | _ -> lst) av []

let objs_of_val av = 
  AVSet.fold (fun v lst -> match v with
                | AObj obj -> obj :: lst
                | _ -> lst) av []

let closures_of_val av = 
  AVSet.fold (fun v lst -> match v with
                | AClosure (n, f, b) -> (n, f, b) :: lst
                | _ -> lst) av []

let closure args body = 
  AVSet.singleton (AClosure (cpsexp_idx body, args, body))

let get_field field_name obj = 
  if IdMap.mem field_name obj then
    IdMap.find field_name obj
  else 
    abs_undef

let strings_of_val av =
  AVSet.fold (fun v lst -> match v with
                  AConst (Exprjs_syntax.CString s) -> s :: lst
                | _ -> lst) av []

let upd_field field_name field_val obj_val =
  IdMap.add field_name field_val obj_val


let val_of_obj (dict : absobj) : absval = AVSet.singleton (AObj dict)

let mk_absobj ps =
  IdMapExt.from_list ps

let any_num = AVSet.singleton ANumber

let any_str = AVSet.singleton AString

let any_bool = AVSet.singleton ABool

let lookup x absenv = IdMap.find x absenv

let bind x v env  = IdMap.add x v env

let empty_absenv = IdMap.empty

let is_any_str avs = AVSet.mem AString avs 

let is_true av = 
  AVSet.mem (AConst (Exprjs_syntax.CBool true)) av ||
  AVSet.mem ABool av


let is_false av = 
  AVSet.mem (AConst (Exprjs_syntax.CBool false)) av ||
  AVSet.mem ABool av

open FormatExt

let rec  p_av av  = match av with
  | AConst c -> Exprjs_pretty.p_const c
  | ARef x -> int x
  | AObj dict ->
      IdMapExt.p_map text (AVSetExt.p_set p_av) dict
  | AArr _ -> text "array"
  | AClosure (n, args, _) -> 
      text ("closure" ^ string_of_int n)
      

let union_env (env1 : AV.env) (env2 : AV.env) : AV.env = 
  IdMapExt.join AVSet.union  env1 env2

let eq_absenv env1 env2 = 
    IdMap.compare AVSet.compare env1 env2 = 0

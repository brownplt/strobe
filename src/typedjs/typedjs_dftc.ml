open Prelude
open Typedjs_stxutil
open Typedjs_syntax
open Typedjs_dfLattice

let rec rt_of_typ (t : typ) : RTSet.t = match t with
    TArrow _ -> RTSet.singleton RTFunction
  | TUnion (t1, t2) -> RTSet.union (rt_of_typ t1, rt_of_typ t2)
  | TApp (s, []) -> 
      (match s with
           "Any" -> any_runtime_type
         | "String" ->  RTSet.singleton RTString
         | "RegExp" -> RTSet.singleton RTObject
         | "Number"  -> RTSet.singleton RTNumber
         | "Int" -> RTSet.singleton RTNumber
         | "Boolean" -> RTSet.singleton RTBoolean
         | "Undefined" -> RTSet.singleton RTUndefined
         | _ -> failwith (sprintf "unknown type: TApp (\"%s\", [])" s))

let runtime (t : typ) : abs_value = AVType (rt_of_typ t)


let annotate (env : typ IdMap.t) (available_ids : IdSet.t) (exp : 'a exp) =
  let anfexp = Typedjs_anf.from_typedjs exp in
    

module Make (Pat : Sig.SET) (Typ : Sig.TYP with module Pat = Pat) : sig
  val unidl : Idl_syntax.definition list -> Typ.typenv
end = struct

  open Prelude
  open Idl_syntax
  open Typ

  let scopedName (name : scopedName) = match name with
    | RelativeName [x] -> Id.string_of_id x
    | _ -> failwith "scopedName : complex name received"

  let rec from_typ (t : Idl_syntax.typ) : Typ.typ = match t with
    | Short _
    | Long _ 
    | LongLong _
    | Octet
    | Byte -> TPrim Int
    | Float
    | Double -> TPrim Num
    | DOMString -> TRegex Pat.all
    | Date -> TPrim Null (* TODO(arjun): wrong *)
    | Any -> TTop
    | Boolean -> TUnion (TPrim True, TPrim False)
    | Object -> TTop
    | Name n -> TId (scopedName n)
    | Void -> TPrim Null
    | Array t' -> TApp (TId "Array", [from_typ t'])
    | Ques t' -> TUnion (from_typ t', TPrim Undef)

  
  let by_member self (member : interfaceMember) (inst_flds, proto_flds) = 
    match member with
    | Attribute (_, _, attr_typ, attr_name) -> (* TODO(arjun): account for RO *)
      let name_pat = Pat.singleton (Id.string_of_id attr_name) in
      ((name_pat, Present, from_typ attr_typ) :: inst_flds, proto_flds)
    | Operation (_, result_typ, method_name, arg_typs) ->
      let name_pat = Pat.singleton (Id.string_of_id method_name) in
      let method_typ = TArrow (self :: (map from_typ arg_typs), 
                               from_typ result_typ) in
      (inst_flds, (name_pat, Present, method_typ) :: proto_flds)
    | ConstMember _ -> (inst_flds, proto_flds) (* TODO(arjun): fill *)
    

  let from_definition defn (trm_vars, typ_vars) = match defn with
    | Module _ -> failwith "module NYI"
    | ForwardInterface _ -> (trm_vars, typ_vars)
    | Const (_, t, x) ->
      (IdMap.add (Id.string_of_id x) (from_typ t) trm_vars, typ_vars)
    | Typedef (_, t, x) -> 
      let typ_op = TLambda ([], from_typ t) in
      (trm_vars, IdMap.add (Id.string_of_id x) (typ_op, KStar) typ_vars)
    | Exception _ -> (trm_vars, typ_vars)
    | Interface (_, name, super, members) ->
      (* An IDL interface defines both a type and a value. *)
      let name = Id.string_of_id name in
      let self = TApp (TId name, []) in
      let (inst_flds, proto_flds) = 
        fold_right (by_member self) members ([],[]) in
      let proto_proto =  match super with
        | None -> TId "Object"
        | Some super -> TId (scopedName super) in
      let proto_flds =
        (Pat.singleton "__proto__", Present, proto_proto) :: proto_flds in
      let proto_absent_pat = 
        Pat.negate (fold_right (fun (pat, _, _) acc -> Pat.union pat acc) 
                    proto_flds Pat.empty) in
      let proto_typ = TRef (TObject (mk_obj_typ proto_flds proto_absent_pat)) in
      let inst_flds = 
        (Pat.singleton "__proto__", Present, proto_typ) :: inst_flds in
      let inst_absent_pat = 
        Pat.negate (fold_right (fun (pat, _, _) acc -> Pat.union pat acc) 
                    inst_flds Pat.empty) in
      let inst_typ_constr = 
        TLambda ([], TRef (TObject (mk_obj_typ inst_flds inst_absent_pat))) in
      let inst_typ_constr_name = "_" ^ name in
      let inst_typ = TApp (TId inst_typ_constr_name, []) in
      (trm_vars, 
       IdMap.add inst_typ_constr_name (inst_typ_constr, KArrow ([], KStar))
         (IdMap.add name (inst_typ, KStar) typ_vars))

  let unidl definitions = 
    let (trm_vars, typ_vars) = 
      fold_right from_definition definitions (IdMap.empty, IdMap.empty) in
     typ_vars

end

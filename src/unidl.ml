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
    | Byte -> TPrim "Num"
    | Float
    | Double -> TPrim "Num"
    | DOMString -> TRegex Pat.all
    | Date -> TPrim "Null" (* TODO(arjun): wrong *)
    | Any -> TTop
    | Boolean -> TUnion (TPrim "True", TPrim "False")
    | Object -> TTop
    | Name n -> TApp (TId (scopedName n), [])
    | Void -> TPrim "Undef" 
    | Array t' -> TApp (TId "Array", [from_typ t'])
    | Ques t' -> TUnion (from_typ t', TPrim "Undef")
    | Sequence t' -> TApp (TId "Array", [from_typ t']) (* TODO(arjun): WRONG *)

  
  let by_member self (member : interfaceMember) (inst_flds, proto_flds) = 
    match member with
    | Attribute (_, _, attr_typ, attr_name) -> (* TODO(arjun): account for RO *)
      let name_pat = Pat.singleton (Id.string_of_id attr_name) in
      ((name_pat, Present, from_typ attr_typ) :: inst_flds, proto_flds)
    | Operation (_, result_typ, method_name, arg_typs) ->
      let name_pat = Pat.singleton (Id.string_of_id method_name) in
      let method_typ = TArrow (self :: (map from_typ arg_typs), None,
                               from_typ result_typ) in
      (inst_flds, (name_pat, Inherited, method_typ) :: proto_flds)
    | _ -> (inst_flds, proto_flds) (* TODO(arjun): fill *)

  let from_interface p name super members metas trm_vars typ_vars =
      (* An IDL interface defines both a type and a value. *)
      let name = Id.string_of_id name in
      if IdMap.mem name typ_vars then
        failwith (sprintf "%s: interface %s redefinition" 
                          (string_of_position (p, p))
                          name);
      let self = TTop in (* TODO(arjun): type self for inheritance *)
      let (inst_flds, proto_flds) = 
        fold_right (by_member self) members ([],[]) in
      let proto_proto =  match super with
        | None -> TId "Object"
        (* TODO(arjun): Set proto correctly for nominality *)
        | Some super -> failwith "What to do here?" (* TApp (TId (scopedName super), []) *)
      in let proto_flds =
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
      let inst_typ = 
        TLambda ([], TRef (TObject (mk_obj_typ inst_flds inst_absent_pat))) in
      (trm_vars, 
       IdMap.add name (inst_typ, KArrow ([], KStar)) typ_vars)

  let from_definition x defn (trm_vars, typ_vars) = match defn with
    | ForwardInterface _
    | Implements  _ 
    | PartialInterface _ -> failwith "should be processed by consolidate"
    | Module _ -> failwith "module NYI"
    | Const (_, t, x) ->
      (IdMap.add (Id.string_of_id x) (from_typ t) trm_vars, typ_vars)
    | Typedef (_, t, x) -> 
      let typ_op = TApp (TLambda ([], from_typ t), []) in
      (trm_vars, IdMap.add (Id.string_of_id x) (typ_op, KStar) typ_vars)
    | Exception _ -> (trm_vars, typ_vars)
    | Interface (p, name, super, members, metas) ->
        if (List.mem Callback metas || List.mem CallbackFunctionOnly metas) then
          let pick_handleEvent mem = match mem with
            | Operation (_, _, _, _) ->  true
            | _ -> false in
          let handler = try 
            List.find pick_handleEvent members
            with Not_found -> raise (Invalid_argument
              (sprintf "%s: no operation in a [Callback]"
                 (string_of_position (p, p)))) in
          begin match handler with
            | Operation (_, rtyp, _,  argtyps) ->
              let name = Id.string_of_id name in
              let t = 
                TLambda ([], 
                  TArrow (TTop :: map from_typ argtyps, None, from_typ rtyp)) in
              (trm_vars, IdMap.add name (t, KArrow ([], KStar)) typ_vars)
            | _ -> failwith "catastrophe in unidl.ml"
          end
        else
          from_interface p name super members metas trm_vars typ_vars
    | Dictionary _ -> (trm_vars, typ_vars) (* TODO(arjun): handle it *)

  let is_partial def = match def with
    | PartialInterface _
    | Implements _ -> true
    | _ -> false

  let consolidate iface_map defn = match defn with
    | Module _ -> failwith "Module NYI"
    | ForwardInterface _ -> iface_map
    | Exception _ -> iface_map (* TODO(arjun): handle it *)
    | Dictionary _ -> iface_map (* TODO(arjun): handle it *)
    | Const (_, _, x)
    | Interface (_, x, _, _, _) 
    | Typedef (_, _, x) -> IdMap.add (Id.string_of_id x) defn iface_map
    | PartialInterface (p, x, ext_mems) ->
      let x_str = Id.string_of_id x in
      begin try  
        match IdMap.find x_str iface_map with
          | Interface (p, x, super, mems, metas) ->
              (* TODO(arjun): semantics of inherited attributes? *)
              IdMap.add x_str (Interface (p, x, super, mems @ ext_mems, metas))
                iface_map
          | _ -> 
            let msg = sprintf "%s: partial interface extends a non-interface"
                        (string_of_position (p, p)) in
            raise (Invalid_argument msg)
      with Not_found ->
        raise (Invalid_argument (sprintf 
          "%s: interface must come before partial interface"
            (string_of_position (p, p))))
      end
    | Implements (p, obj, trait) ->
      let obj_str = Id.string_of_id obj in
      try match IdMap.find obj_str iface_map,
                IdMap.find (Id.string_of_id trait) iface_map with
            | Interface (p1, obj_name, super, mems, metas),
              Interface (p2, _, None, trait_mems, _) ->
                IdMap.add obj_str 
                  (Interface (p1, obj_name, super, mems @ trait_mems, metas))
                  iface_map
            | _ -> raise (Invalid_argument (sprintf "%s: bad implements clause"
                     (string_of_position (p, p))))
      with Not_found ->
        raise (Invalid_argument (sprintf "%s: not found" 
          (string_of_position (p, p))))

  let rec collapse x _ iface_map = match IdMap.find x iface_map with
    | Interface (p, name, Some super, mems, metas) as rhs ->
      let super = scopedName super in
      let iface_map = collapse super rhs iface_map in
      begin match IdMap.find super iface_map with
        | Interface (_, _, None, super_mems, _) -> 
            IdMap.add x 
              (Interface (p, name, None, mems @ super_mems, metas)) iface_map
        | _ -> failwith "collapse failed"
      end
    | _ -> iface_map

  let unidl definitions = 
    let iface_map =
       let (parts, mains) = List.partition is_partial definitions in
       fold_left consolidate (fold_left consolidate IdMap.empty mains) parts in
    let iface_map = IdMap.fold collapse iface_map iface_map in
    let (trm_vars, typ_vars) = 
      IdMap.fold from_definition iface_map (IdMap.empty, IdMap.empty) in
    IdMap.add "BrowserGlobal" (TApp (TId "Window", []), KStar) typ_vars

end

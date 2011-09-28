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
    | Name n -> TApp (TId (scopedName n), [])
    | Void -> TPrim Undef (* TUnion (TPrim Null, TPrim Undef) *)
    | Array t' -> TApp (TId "Array", [from_typ t'])
    | Ques t' -> TUnion (from_typ t', TPrim Undef)
    | Sequence t' -> TApp (TId "Array", [from_typ t']) (* TODO(arjun): WRONG *)

  
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
        | Some super -> TApp (TId (scopedName super), []) in
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
      let inst_typ = 
        TLambda ([], TRef (TObject (mk_obj_typ inst_flds inst_absent_pat))) in
      (trm_vars, 
       IdMap.add name (inst_typ, KArrow ([], KStar)) typ_vars)

  let from_definition (trm_vars, typ_vars) defn = match defn with
    | Module _ -> failwith "module NYI"
    | ForwardInterface _ -> (trm_vars, typ_vars)
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
                  TArrow (TTop :: map from_typ argtyps, from_typ rtyp)) in
              (trm_vars, IdMap.add name (t, KArrow ([], KStar)) typ_vars)
            | _ -> failwith "catastrophe in unidl.ml"
          end
        else
          from_interface p name super members metas trm_vars typ_vars
    | Implements (p, obj_name, super_name) ->
      let obj_name = Id.string_of_id obj_name in
      let super_name = Id.string_of_id super_name in
      if not (IdMap.mem obj_name typ_vars) then
        failwith (sprintf "%s: %s implements ... does not exist" 
                          (string_of_position (p, p))
                          obj_name );
      if not (IdMap.mem super_name typ_vars) then
        failwith (sprintf "%s: ... implements %s does not exist" 
                          (string_of_position (p, p))
                          super_name );
      let (obj_typ, _) = IdMap.find obj_name typ_vars in
      let (super_typ, _) = IdMap.find super_name typ_vars in
      begin match (obj_typ, super_typ) with
        | TLambda ([], TRef (TObject obj)), TLambda ([], TRef (TObject sup)) ->
          (* TODO(arjun): remove __proto__ from sup? *)
          let flds = fields obj @ fields sup in
          let absent_pat = 
            Pat.negate (fold_right (fun (pat, _, _) acc -> Pat.union pat acc) 
                       flds Pat.empty) in
          let obj_typ = 
            TLambda ([], TRef (TObject (mk_obj_typ flds absent_pat))) in
          (trm_vars, IdMap.add obj_name (obj_typ, KArrow ([], KStar)) typ_vars)
        | _ -> failwith "strange types in implements"
      end
  | PartialInterface (p, name, members) ->
      (* An IDL interface defines both a type and a value. *)
      let name = Id.string_of_id name in
      let self = TId name in
      let (inst_flds, proto_flds) =  (* TODO(arjun): proto_flds, handle it *)
        fold_right (by_member self) members ([],[]) in
      if not (IdMap.mem name typ_vars) then
        failwith (sprintf "%s: partial interface %s does not exist" 
                          (string_of_position (p, p)) name);
      begin match IdMap.find name typ_vars with
        | (TLambda ([], TRef (TObject obj)), KArrow ([], KStar)) ->
            let inst_flds = fields obj @ inst_flds in
            let absent_pat = 
              Pat.negate (fold_right (fun (pat, _, _) acc -> Pat.union pat acc) 
                         inst_flds Pat.empty) in
            let obj_typ = 
              TLambda ([], TRef (TObject (mk_obj_typ inst_flds absent_pat))) in
            (trm_vars, IdMap.add name 
                                 (obj_typ, KArrow ([], KStar)) typ_vars)
        | _ -> failwith "strange types in partial interface"
      end
  | Dictionary _ -> (trm_vars, typ_vars) (* TODO(arjun): handle it *)

  let is_partial def = match def with
    | PartialInterface _
    | Implements _ -> true
    | _ -> false

  let unidl definitions = 
    let (partials, mains) = List.partition is_partial definitions in
    let (trm_vars, typ_vars) = 
      (* fold_left to process top->bottom. *)
      fold_left from_definition
        (fold_left from_definition (IdMap.empty, IdMap.empty) mains)
        partials in
     IdMap.add "BrowserGlobal" (TApp (TId "Window", []), KStar) typ_vars

end

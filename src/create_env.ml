open Prelude
open Full_idl_syntax
open Format
open FormatExt
open Typedjs_syntax
open TypImpl
open ListExt

let sort_defs defs = List.stable_sort (fun d1 d2 -> 
  let order def = match def with
    | Include _ -> 0
    | Typedef _ -> 1
    | ForwardInterface _ -> 2
    | Enum _ -> 3
    | Callback _ -> 4
    | Const _ -> 5
    | Exception _ -> 6
    | Dictionary _ -> 7
    | Module _ -> 8
    | Interface _ -> 9
    | PartialInterface _ -> 10
    | Implements _ -> 11
  in match d1, d2 with
  | Module (_, _, id1, _), Module (_, _, id2, _) 
  | Typedef (_, _, _, id1), Typedef (_, _, _, id2)
  | Interface (_, _, id1, _, _, _), Interface (_, _, id2, _, _, _)
  | ForwardInterface (_, _, id1), ForwardInterface (_, _, id2)
  | Exception (_, _, id1, _, _), Exception (_, _, id2, _, _)
  | Implements (_, _, id1, _), Implements (_, _, id2, _)
  | Const (_, _, _, id1, _), Const (_, _, _, id2, _)
  | Dictionary (_, _, id1, _, _), Dictionary (_, _, id2, _, _)
  | PartialInterface (_, _, id1, _), PartialInterface (_, _, id2, _)
  | Callback (_, _, id1, _, _), Callback (_, _, id2, _, _)
  | Enum (_, _, id1, _), Enum (_, _, id2, _) -> compare (Id.string_of_id id1) (Id.string_of_id id2)
  | Include (_, _, file1), Include (_, _, file2) -> compare file1 file2
  | _, _ -> compare (order d1) (order d2)
) defs


type cachedInterface = Raw of Full_idl_syntax.definition | Comp of TypImpl.typ
let create_env defs = 
  let ifaceHash : (Id.t, cachedInterface) Hashtbl.t = Hashtbl.create 1200 in
  let idToPat id = P.singleton (Id.string_of_id id) in
  let isNoScript metas = List.exists (fun m -> m = NoScript || m = NotXPCOM) metas in
  let isScriptable metas = List.exists (fun m -> m = Scriptable) metas in
  let isPrivateBrowsingCheck metas = List.exists (fun m -> m = PrivateBrowsingCheck) metas in
  let isUnsafe metas = List.exists (fun m -> m = Unsafe) metas in
  let isQueryInterfaceType metas = List.exists (fun m -> m = QueryInterfaceType) metas in
  let isRetval metas = List.exists (fun m -> m = Retval) metas in
  let rec trans_toplevel defs =
    let (iids, componentsAndCID, compType, queryInterfaceType) = build_components (defs) in
    List.iter (fun i -> match i with
    | Interface (_, _, name, parent, members, _) -> Hashtbl.add ifaceHash name (Raw i)
    | _ -> ()) defs;
    let interfaces = (filter_map (trans_def TBot queryInterfaceType) (defs)) in
    let iids = filter_map (fun iid ->
      let iid_hashname = Id.id_of_string ("##" ^ (iid) ^ "##") in
      let iid_iface = Interface(Lexing.dummy_pos, [], Id.id_of_string iid, 
                                Some (RelativeName [Id.id_of_string "nsIJSIID"]),
                                [Attribute(Lexing.dummy_pos, [], NoReadOnly, IsNormal, Any, iid_hashname)], 
                                IsNormalInterface) in
      Hashtbl.add ifaceHash (Id.id_of_string iid) (Raw iid_iface);
      trans_interface TBot queryInterfaceType (Id.id_of_string iid)
    ) iids in
    ((componentsAndCID :: iids @ interfaces), TId "Components")
  (* and trans_defs tt defs =  *)
  (*   let (_, componentsAndCID, _, queryInterfaceType) = build_components (defs) in *)
  (*   let interfaces = (filter_map (trans_def tt queryInterfaceType) (defs)) in *)
  (*   let allFields = componentsAndCID :: interfaces in *)
  (*   let catchallPat = match allFields with *)
  (*     | [] -> P.all *)
  (*     | hd::tl -> P.negate (List.fold_left P.union (fst3 hd) (List.map fst3 tl)) in *)
  (*   (\*TSource*\)TRef ((\* TRef *\) (TObject (mk_obj_typ allFields catchallPat))) *)
  and wrapArrow t =
    let codePat = P.singleton "-*- code -*-" in
    let prototypePat = P.singleton "prototype" in
    TRef(TObject(mk_obj_typ [(codePat, Present, t);
                             (proto_pat, Present, TId "Object");
                             (prototypePat, Present, TId "Ext");
                             ((P.negate (P.union (P.union codePat prototypePat) proto_pat)), Maybe, TId "Ext")]
                   P.empty))
  and build_components defs = 
    let interfaceToIIDFieldAndFun def =
      match def with
      | Interface (_, metas, name, _, _, _) ->
        if (not (isScriptable metas))
        then None
        else let iidName = (Id.string_of_id name) ^ "_IID" in
             Some (iidName,
                   (idToPat name, Present, TId iidName),
                   (fun tself -> TArrow ([TId "nsISupports"; TId iidName], None, (TId (Id.string_of_id name)))))
      | _ -> None in
    let unzip3 abcs =
      let rec helper abcs aas bbs ccs =
        match abcs with
        | [] -> (List.rev aas, List.rev bbs, List.rev ccs)
        | (a,b,c) :: abcs' -> helper abcs' (a :: aas) (b :: bbs) (c :: ccs) in
      helper abcs [] [] [] in
    let (iids, fields, funs) = unzip3 (filter_map interfaceToIIDFieldAndFun defs) in
    let proto = (proto_pat, Present, TId "Object") in
    let compInterface = TSource(*TRef*) (TObject (mk_obj_typ (proto::fields) P.empty)) in
    let compUtils = TSource(*TRef*) (TObject (mk_obj_typ [(P.singleton "import", Present, wrapArrow (TArrow([TTop; TRegex P.all], None, TPrim "Undef")))] P.empty)) in
    let compID = wrapArrow(TArrow([TTop; TRegex P.all], None, TId "Ext")) in
    let allOthers = P.negate proto_pat in
    (* NOTE: This should be Maybe, not Present, but the type system complains if I do that... *)
    let compClasses = TSource(*TRef*) (
                                 (TObject (mk_obj_typ [(allOthers, Present, (TId "nsIJSCID")); proto] P.empty))) in
    let componentsType = 
      TSource(*TRef*) ((* TRef *) (TObject (mk_obj_typ [(P.singleton "interfaces", Present, compInterface);
                                                (P.singleton "classes", Present, compClasses);
                                                (P.singleton "utils", Present, compUtils);
                                                (P.singleton "ID", Present, compID);
                                                proto] P.empty))) in
    let queryInterfaceType tself =
      wrapArrow (List.fold_left (fun acc f -> TIntersect (f tself, acc))
                   (TArrow ([tself; TId "nsIJSIID"], None, TId "nsISupports")) funs) in
    (* let queryInterfaceType tself = match funs with *)
    (*   | [] -> TBot (\* absurd *\) *)
    (*   | [ty] -> wrapArrow (ty tself) *)
    (*   | f::fs -> wrapArrow (List.fold_left (fun acc f -> TIntersect (f tself, acc)) (f tself) fs) in *)
    (iids,
     (P.singleton "Components", Present, componentsType),
     componentsType, queryInterfaceType)
  and trans_def tt queryInterfaceType def = match def with
    | Module (_, metas, name, defs) -> None (* Some (idToPat name, Present, trans_defs tt defs) *) 
    (* NOT YET SUPPORTED *)
    | Typedef _ -> None
    | Interface (_, _, name, _, _, _) -> trans_interface tt queryInterfaceType name 
    | ForwardInterface _ -> None
    | Exception _
    | Implements _ -> None
    | Const _  
    | Dictionary _
    | PartialInterface _
    | Callback _
    | Enum _
    | Include _ -> None
  and trans_interface tt queryInterfaceType name =
    match Hashtbl.find ifaceHash name with
    | Comp typ -> Some (idToPat name, Inherited, typ)
    | Raw (Interface(_, metas, name, parent, members, isCallback)) ->
      let tself = (TId (Id.string_of_id name)) in
      let transfields = trans_fields tself (queryInterfaceType tself) members in
      let parentTyp = match parent with
          | None -> None
          | Some pName -> match pName with
            | RelativeName [id] -> trans_interface tt queryInterfaceType id
            | _ -> failwith "absurd, won't happen" in
      let parentFields = match parentTyp with
        | Some (_, _, TSource(*TRef*)(TObject f)) -> TypImpl.fields f
        | _ -> [(proto_pat, Present, (TId "Object"))] in
      let transfields = transfields @ parentFields in
      let catchallPat = P.empty in (*P.negate (List.fold_left P.union proto_pat (List.map fst3 transfields)) in*)
      let ifaceTyp = TSource(*TRef*) (TObject (mk_obj_typ transfields catchallPat)) in
      Hashtbl.add ifaceHash name (Comp ifaceTyp);
      Some(idToPat name, Inherited, ifaceTyp)
    | Raw _ -> None
  and trans_fields tt queryInterfaceType fields = filter_map (trans_field tt queryInterfaceType) fields
  and trans_field tself queryInterfaceType field = match field with
    | Attribute (_, metas, readOnly, stringifier, typ, name) ->
      if (isNoScript metas) then None
      else if (isPrivateBrowsingCheck metas) then Some (idToPat name, Present, (TPrim "True"))
      else if (isUnsafe metas) then Some (idToPat name, Present, TPrim "Unsafe")
      else if (isQueryInterfaceType metas) then Some (idToPat name, Present, queryInterfaceType)
      else let t = trans_typ typ in 
           let t = match readOnly with
             | NoReadOnly -> t
             | ReadOnly -> TSource t in
           Some (idToPat name, Inherited, t)
    | Operation (_, metas, stringifier, quals, typ, nameOpt, args) -> 
      let returnField typ = match nameOpt with None -> None | Some name -> Some (idToPat name, Inherited, typ) in
      if (isNoScript metas) then None
      else if (isUnsafe metas) then returnField (TPrim "Unsafe")
      else if (isQueryInterfaceType metas) then returnField queryInterfaceType
      else returnField
        (match List.rev args with
        | [] -> wrapArrow (TArrow ([tself], None, trans_typ typ))
        | lastArg::revArgs ->
        let (_, lastMetas, lastTyp, _, _, _) = lastArg in
        if (isRetval lastMetas) 
        then wrapArrow (TArrow (tself :: List.map (trans_arg tself) (List.rev revArgs), None, trans_typ lastTyp))
        else wrapArrow (TArrow (tself :: List.map (trans_arg tself) args, None, trans_typ typ))
        )
    | ConstMember (_, metas, typ, id, value) -> 
      if (isNoScript metas)
      then None
      else Some (idToPat id, Inherited, TSource (trans_typ typ))
    | Stringifier (_, metas) -> None
  and trans_arg tt (direction, metas, typ, variadic, id, defaultOpt) = 
    match direction with
    | InParam -> trans_typ typ
    | OutParam -> 
      TRef (TObject (mk_obj_typ [(P.singleton "value", Present, TSink (trans_typ typ))] P.empty))
    | InOutParam -> 
      TRef (TObject (mk_obj_typ [(P.singleton "value", Present, (trans_typ typ))] P.empty))
  and trans_typ typ = match typ with
    | Short Unsigned 
    | Short NoUnsigned
    | Long Unsigned
    | Long NoUnsigned
    | LongLong Unsigned
    | LongLong NoUnsigned 
    | Byte 
    | Octet
    | Float
    | Double -> TPrim "Num"
    | Boolean -> typ_bool
    | DOMString -> TRegex P.all
    | Date -> TId "Date"
    | Object -> TRef (TObject (mk_obj_typ [] P.all))
    | Any -> TTop
    | Void -> TPrim "Undef"
    | Ques t -> TUnion (TPrim "Null", trans_typ t)
    | Native s -> TId s
    | Name n -> begin match n with
      | RelativeName [id] -> begin match (Id.string_of_id id) with
        | "wstring"
        | "string"
        | "wchar"
        | "char" -> TRegex P.all
        | id -> TId id
      end
      | _ -> TId "##unknown##"
    end
    | Array t
    | Sequence t -> TApp (TId "Array", [trans_typ t])
  in (trans_toplevel defs)


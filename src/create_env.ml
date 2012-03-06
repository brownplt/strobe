open Prelude
open Full_idl_syntax
open Format
open FormatExt
open Typedjs_syntax


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



let create_env defs = 
  let idToPat id = P.singleton (Id.string_of_id id) in
  let filterNone xs = (* 'a option list -> 'a list *)
    let rec helper xs acc = match xs with
      | [] -> List.rev acc
      | Some x :: xs -> helper xs (x :: acc)
      | None :: xs -> helper xs acc
    in helper xs [] in
  let isNoScript metas = List.exists (fun m -> m = NoScript || m = NotXPCOM) metas in
  let isScriptable metas = List.exists (fun m -> m = Scriptable) metas in
  let isPrivateBrowsingCheck metas = List.exists (fun m -> m = PrivateBrowsingCheck) metas in
  let isUnsafe metas = List.exists (fun m -> m = Unsafe) metas in
  let isRetval metas = List.exists (fun m -> m = Retval) metas in
  let rec trans_toplevel defs =
    let interfaces = (filterNone (List.map (trans_def TBot) (sort_defs defs))) in
    let (iids, componentsAndCID) = build_components (sort_defs defs) in
    (iids, 
     List.fold_left (fun map (name, _, t) -> 
       match (P.singleton_string name) with
       | Some name -> IdMap.add name t map
       | None -> map (* BSL: Should never happen *)) IdMap.empty (interfaces @ componentsAndCID))
  and trans_defs tt defs = 
    let interfaces = (filterNone (List.map (trans_def tt) (sort_defs defs))) in
    let (_, componentsAndCID) = build_components (sort_defs defs) in
    TSource (TRef (TObject (mk_obj_typ (interfaces @ componentsAndCID) P.all)))
  and build_components defs = 
    let interfaceToIIDFieldAndFun def =
      match def with
      | Interface (_, metas, name, _, _, _) ->
        if (not (isScriptable metas))
        then None
        else let iidName = (Id.string_of_id name) ^ "_IID" in
             Some (iidName,
                   (idToPat name, Present, TPrim iidName),
                   TArrow ([TId "nsIJSCID"; TPrim iidName], TSource (TId (Id.string_of_id name))))
      | _ -> None in
    let unzip3 abcs =
      let rec helper abcs aas bbs ccs =
        match abcs with
        | [] -> (List.rev aas, List.rev bbs, List.rev ccs)
        | (a,b,c) :: abcs' -> helper abcs' (a :: aas) (b :: bbs) (c :: ccs) in
      helper abcs [] [] [] in
    let (iids, fields, funs) = unzip3 (filterNone (List.map interfaceToIIDFieldAndFun defs))in
    let proto = (proto_pat, Present, TId "Object") in
    let compInterface = TSource (TRef (TObject (mk_obj_typ (proto::fields) P.empty))) in
    let allOthers = P.negate proto_pat in
    (* NOTE: This should be Maybe, not Present, but the type system complains if I do that... *)
    let compClasses = TSource (
                                 (TObject (mk_obj_typ [(allOthers, Maybe, (TId "nsIJSCID")); proto] P.empty))) in
    let componentsType = 
      TSource (TRef (TObject (mk_obj_typ [(P.singleton "interfaces", Present, compInterface);
                                          (P.singleton "classes", Present, compClasses);
                                          proto] P.empty))) in
    let queryInterfaceType = match funs with
      | [] -> TBot (* absurd *)
      | [ty] -> ty
      | f::fs -> List.fold_left (fun f acc -> TIntersect (f, acc)) f fs in
    let jscidType = 
      TSource (TRef (TObject (mk_obj_typ 
                                [(P.singleton "getService", Present, queryInterfaceType);
                                 (P.singleton "createInstance", Present, queryInterfaceType);
                                 (proto_pat, Present, TSource (TId "nsIJSID"))]
                                P.empty))) in
    (iids,
     [(P.singleton "Components", Present, componentsType);
      (P.singleton "nsIJSCID", Present, jscidType)])
  and trans_def tt def = match def with
    | Module (_, metas, name, defs) -> Some (idToPat name, Present, trans_defs tt defs)
    | Typedef _ -> None
    | Interface (_, metas, name, parent, members, isCallback) -> 
      let transfields = trans_fields (TId (Id.string_of_id name)) members in
      let ifaceTyp = match parent with
        | None -> 
          let proto = (proto_pat, Present, TSource (TId "Object")) in
          TSource (TRef (TObject (mk_obj_typ (proto::transfields) P.all)))
        | Some pName -> match pName with
          | RelativeName [id] -> 
            TSource (TRef (TObject (mk_obj_typ ((proto_pat, Present, TSource (TId (Id.string_of_id id)))
                                                :: transfields) P.all)))
          | _ -> TSource (TRef (TObject (mk_obj_typ transfields P.all))) (* absurd, won't happen *)
      in
      Some (idToPat name, Present, ifaceTyp)
    | ForwardInterface _ -> None
    | Exception _
    | Implements _ -> None
    | Const _  
    | Dictionary _
    | PartialInterface _
    | Callback _
    | Enum _
    | Include _ -> None
  and trans_fields tt fields = filterNone (List.map (trans_field tt) fields)
  and trans_field tt field = match field with
    | Attribute (_, metas, readOnly, stringifier, typ, name) ->
      if (isNoScript metas) then None
      else if (isPrivateBrowsingCheck metas) then Some (idToPat name, Present, TSource (TPrim "True"))
      else if (isUnsafe metas) then Some (idToPat name, Present, TPrim "Unsafe")
      else let t = trans_typ tt typ in 
           let t = match readOnly with
             | NoReadOnly -> t
             | ReadOnly -> TSource t in
           Some (idToPat name, Present, t)
    | Operation (_, metas, stringifier, quals, typ, nameOpt, args) -> 
      if (isNoScript metas) then None
      else if (isUnsafe metas) then 
        (match nameOpt with None -> None | Some name -> Some (idToPat name, Present, TPrim "Unsafe"))
      else begin match nameOpt with
      | None -> None (* return to this *)
      | Some name -> 
        match List.rev args with
        | [] -> Some (idToPat name, Present, TArrow ([tt], trans_typ tt typ))
        | lastArg::revArgs ->
        let (_, lastMetas, lastTyp, _, _, _) = lastArg in
        if (isRetval lastMetas) 
        then Some (idToPat name, Present,
                   TArrow (tt :: List.map (trans_arg tt) (List.rev revArgs), trans_typ tt lastTyp))
        else Some (idToPat name, Present, 
                   TArrow (tt :: List.map (trans_arg tt) args, trans_typ tt typ))
      end
    | ConstMember (_, metas, typ, id, value) -> 
      if (isNoScript metas)
      then None
      else Some (idToPat id, Present, TSource (trans_typ tt typ))
    | Stringifier (_, metas) -> None
  and trans_arg tt (direction, metas, typ, variadic, id, defaultOpt) = 
    match direction with
    | InParam -> trans_typ tt typ
    | OutParam -> 
      TRef (TObject (mk_obj_typ [(P.singleton "value", Present, TSink (trans_typ tt typ))] P.empty))
    | InOutParam -> 
      TRef (TObject (mk_obj_typ [(P.singleton "value", Present, (trans_typ tt typ))] P.empty))
  and trans_typ tt typ = match typ with
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
    | Boolean -> TUnion (TPrim "True", TPrim "False")
    | DOMString -> TId "DOMString"
    | Date -> TId "Date"
    | Object -> TRef (TObject (mk_obj_typ [] P.all))
    | Any -> TTop
    | Void -> TPrim "Undef"
    | Ques t -> TUnion (TPrim "Null", trans_typ tt t)
    | Native s -> TId s
    | Name n -> begin match n with
      | RelativeName [id] -> TId (Id.string_of_id id)
      | _ -> TId "unknown"
    end
    | Array _
    | Sequence _ -> TId "unknown"
  in (trans_toplevel defs)


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


let parseType typStr =
  let open Lexing in
  let start_pos = { pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  let len = String.length typStr in
  let end_pos = { pos_fname = "<string>"; pos_lnum = 1; pos_bol = len; pos_cnum = len } in
  match Typedjs_fromExpr.parse_annotation (start_pos, end_pos) typStr with
  | Typedjs_syntax.ATyp typ ->
    Sb_desugar.desugar_typ (start_pos, end_pos) typ
  | _ -> raise (Invalid_argument "Didn't get a valid type")


type cachedInterface = 
  | Raw of Full_idl_syntax.definition * bool
  | Comp of TypImpl.typ * (TypImpl.typ -> TypImpl.typ) option (* interface type, type as a [function] *)
let create_env defs = 
  let ifaceHash : (Id.t, cachedInterface) Hashtbl.t = Hashtbl.create 1200 in
  let implementsHash : (Id.t, Id.t) Hashtbl.t = Hashtbl.create 200 in
  let idToPat id = P.singleton (Id.string_of_id id) in
  let find_in_list l v = List.exists (fun m -> m = v) l in
  let isNoScript metas = find_in_list metas NoScript || find_in_list metas NotXPCOM in
  let isArray metas = find_in_list metas (AttrNoArgs (Id.id_of_string "array")) in
  let isScriptable metas = find_in_list metas Scriptable in
  let isPrivateBrowsingCheck metas = find_in_list metas PrivateBrowsingCheck  in
  let isUnsafe metas = find_in_list metas Unsafe in
  let isGetter metas = find_in_list metas (AttrNoArgs (Id.id_of_string "getter")) in
  let isFunction metas = find_in_list metas (AttrNoArgs (Id.id_of_string "function")) in
  let isOptional metas = find_in_list metas Optional in
  let isQueryInterfaceType metas = find_in_list metas QueryInterfaceType in
  let isQueryElementAtType metas = 
    try Some (List.find (fun m -> match m with QueryElementAtType _ -> true | _ -> false) metas)
    with Not_found -> None in
  let isUseType metas =
    try Some (List.find (fun m -> match m with AttrArgList(id, _) -> id = Id.id_of_string "UseType" | _ -> false) metas)
    with Not_found -> None in
  let isRetval metas = List.exists (fun m -> m = Retval) metas in
  let rec trans_toplevel defs =
    let (iidsAndConstants, componentsAndCID, compType, queryInterfaceType) = build_components (defs) in
    List.iter (fun i -> match i with
    | Interface (_, metas, name, parent, members, _) -> Hashtbl.add ifaceHash name (Raw (i, isFunction metas))
    | Implements (_, _, id1, id2) -> Hashtbl.add implementsHash id1 id2
    | _ -> ()) defs;
    let interfaces = filter_map (trans_def TBot queryInterfaceType) (defs) in
    let iids = filter_map (fun (iid, constants) ->
      let iid_hashname = Id.id_of_string ("##" ^ (iid) ^ "##") in
      let iid_iface = Interface(Lexing.dummy_pos, [], Id.id_of_string iid, 
                                Some (RelativeName [Id.id_of_string "nsIJSIID"]),
                                ((Attribute(Lexing.dummy_pos, [], NoReadOnly, IsNormal, Any, iid_hashname)) 
                                 :: constants), 
                                IsNormalInterface) in
      Hashtbl.add ifaceHash (Id.id_of_string iid) (Raw (iid_iface, false));
      trans_interface TBot queryInterfaceType (Id.id_of_string iid)
    ) iidsAndConstants in
    ((componentsAndCID :: iids @ interfaces), TId "Components", queryInterfaceType (TId "nsISupports") 0)
  (* and trans_defs tt defs =  *)
  (*   let (_, componentsAndCID, _, queryInterfaceType) = build_components (defs) in *)
  (*   let interfaces = (filter_map (trans_def tt queryInterfaceType) (defs)) in *)
  (*   let allFields = componentsAndCID :: interfaces in *)
  (*   let catchallPat = match allFields with *)
  (*     | [] -> P.all *)
  (*     | hd::tl -> P.negate (P.unions ((fst3 hd)::(List.map fst3 tl))) in *)
  (*   (\*TSource*\)TRef ((\* TRef *\) (TObject (mk_obj_typ allFields catchallPat))) *)
  and wrapArrow mutability t =
    let codePat = P.singleton "-*- code -*-" in
    let prototypePat = P.singleton "prototype" in
    let obj = (TObject(mk_obj_typ [(codePat, Present, t);
                                   (proto_pat, Present, TId "Object");
                                   (prototypePat, Present, TId "Ext");
                                   ((P.negate (P.unions [codePat; prototypePat; proto_pat])), 
                                    Maybe, TId "Ext")]
                   P.empty)) in
    match mutability with
    | RefCell -> TRef (None, obj)
    | SourceCell -> TSource (None, obj)
    | SinkCell -> TSink (None, obj)
  and unwrapArrow typ = 
    let codePat = P.singleton "-*- code -*-" in
    let prototypePat = P.singleton "prototype" in
    match typ with
    | TRef(_, t)
    | TSource(_, t)
    | TSink(_, t) -> begin match t with
      | TObject(flds) -> begin 
        let fieldList = fields flds in
        let findField namePat =
          try
            let (_, _, t) =
              List.find (fun (n, p, _) -> (p = Present && n = namePat)) fieldList in
            (true, Some t)
          with Not_found -> (false, None) in
        let (hasProto, _) = findField proto_pat in
        let (hasCode, codeTyp) = findField codePat in
        let (hasPrototype, protoTyp) = findField prototypePat in
        let isSimplePrototype = match protoTyp with
          | Some (TId t) -> t = "Object" || t = "Any" || t = "Ext"
                                              || (String.length t > 3 && String.sub t 0 3 = "nsI")
          | _ -> false in
        if ((List.length fieldList) = 4 && hasProto && hasCode && hasPrototype && isSimplePrototype)
        then match codeTyp with Some t -> t | None -> typ
        else typ 
      end
      | _ -> typ
    end
    | _ -> typ
  and build_components defs = 
    let interfaceToIIDFieldAndFun def =
      match def with
      | Interface (_, metas, name, _, members, _) ->
        if (not (isScriptable metas))
        then None
        else let iidName = (Id.string_of_id name) ^ "_IID" in
             let constants = filter_map (fun m -> match m with ConstMember _ -> Some m | _ -> None) members in
             Some ((iidName, constants),
                   (idToPat name, Present, TId iidName),
                   (fun tself n -> 
                     TArrow (((TThis (TId "nsISupports")) :: (ListExt.create n TTop) @ [TId iidName]),
                                           None, (TApp(TPrim "Mutable", [TId (Id.string_of_id name)])))))
      | _ -> None in
    let unzip3 abcs =
      let rec helper abcs aas bbs ccs =
        match abcs with
        | [] -> (List.rev aas, List.rev bbs, List.rev ccs)
        | (a,b,c) :: abcs' -> helper abcs' (a :: aas) (b :: bbs) (c :: ccs) in
      helper abcs [] [] [] in
    let (iids, fields, funs) = unzip3 (filter_map interfaceToIIDFieldAndFun defs) in
    let proto = (proto_pat, Present, TId "Object") in
    let compInterface = TSource(*TRef*) (Some "Components.interfaces", 
                                         TObject (mk_obj_typ (proto::fields) P.empty)) in
    let compUtils = TSource(*TRef*) (Some "Components.utils",
                                     TObject (mk_obj_typ [(P.singleton "import", Present, 
                                                           wrapArrow SourceCell (TArrow([TTop; TRegex P.all], 
                                                                                        None, TPrim "Undef")));
                                                          (P.singleton "reportError", Present,
                                                          wrapArrow SourceCell (TArrow([TTop; TTop], 
                                                                                       None, TPrim "Undef")))]
                                                P.empty)) in
    let compID = wrapArrow SourceCell (TArrow([TTop; TRegex P.all], None, TId "Ext")) in
    let allOthers = P.negate proto_pat in
    (* NOTE: This should be Maybe, not Present, but the type system complains if I do that... *)
    let compClasses = TSource(*TRef*) (Some "Components.classes",
                                       (TObject (mk_obj_typ [(allOthers, Present, (TId "nsIJSCID")); proto] P.empty))) in
    let componentsType = 
      TSource(* TRef *) (Some "Components",
                         TObject (mk_obj_typ [(P.singleton "interfaces", Present, compInterface);
                                              (P.singleton "classes", Present, compClasses);
                                              (P.singleton "utils", Present, compUtils);
                                              (P.singleton "ID", Present, compID);
                                              proto] P.empty)) in
    let queryInterfaceType tself nArgs =
      wrapArrow SourceCell (List.fold_left (fun acc f -> TIntersect (Some "QIType", f tself nArgs, acc))
                              (TArrow (((TThis tself) :: (ListExt.create nArgs TTop) @ [TId "nsIJSIID"]), None, TId "nsISupports")) funs) in
    (* let queryInterfaceType tself nArgs = match funs with *)
    (*   | [] -> TBot (\* absurd *\) *)
    (*   | [ty] -> wrapArrow (ty nArgs tself) *)
    (*   | f::fs -> wrapArrow (List.fold_left (fun acc f -> TIntersect (f tself nArgs, acc)) (f tself nArgs) fs) in *)
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
    | Comp (typ, _) -> Some (idToPat name, Inherited, typ)
    | Raw (Interface(_, metas, name, parent, members, isCallback), _) ->
      let tself = (TId (Id.string_of_id name)) in
      let transfields = trans_fields tself (queryInterfaceType tself) members in
      (* deal with [function] interfaces *)
      let funcFunc = if not (isFunction metas) then None else
          let operationTypes = filter_map (fun (_, _, t) -> match t with
            | TSource(*TRef*)(_, TObject f) -> (match TypImpl.fields f with
              | [(_, Present, t); (_, Present, TId "Object"); (_, Present, TId "Ext"); (_, Maybe, TId "Ext")] ->
                Some t
              | _ -> None)
            | _ -> None) transfields in
          let rec allEqual ts = match ts with 
            | [] -> false
            | [t] -> true
            | t1::t2::ts -> t1 = t2 && allEqual (t2::ts) in
          let operationTypes = if allEqual operationTypes then [List.hd operationTypes] else operationTypes in
          match operationTypes with
          | [TArrow(_::args, vararg, ret)] -> Some (fun tthis -> TArrow((TThis tthis)::args, vararg, ret))
          | [TArrow([], vararg, ret)] -> Some (fun tthis -> TArrow([TThis tthis], vararg, ret))
          | _ -> None (* no (or no unique) operations on the object, despite being [function] *)
      in
      (* deal with inherited fields *)
      let parentTyp = match parent with
          | None -> None
          | Some pName -> match pName with
            | RelativeName [id] -> trans_interface tt queryInterfaceType id
            | _ -> failwith "absurd, won't happen" in
      let parentFields = match parentTyp with
        | Some (_, _, TSource(*TRef*)(_, TObject f)) -> TypImpl.fields f
        | _ -> [(proto_pat, Present, (TId "Object"))] in
      let implementsIds = Hashtbl.find_all implementsHash name in
      let implementsTyps = ListExt.filter_map (trans_interface tt queryInterfaceType) implementsIds in
      let implementsFields = List.concat (List.map (fun t -> match t with
        | (_, _, TSource(*TRef*)(_, TObject f)) -> TypImpl.fields f
        | _ -> []) implementsTyps) in
      let transfields = transfields @ parentFields @ implementsFields in
      let catchallPat = P.empty in (*P.negate (P.unions (proto_pat::(List.map fst3 transfields))) in*)
      let ifaceTyp = TSource(*TRef*) (Some (Id.string_of_id name), TObject (mk_obj_typ transfields catchallPat)) in
      Hashtbl.add ifaceHash name (Comp (ifaceTyp, funcFunc));
      Some(idToPat name, Inherited, ifaceTyp)
    | Raw _ -> None
  and trans_fields tt queryInterfaceType fields = 
    let trans_fields = List.flatten (filter_map (trans_field tt queryInterfaceType) fields) in
    let (opers, others) = 
      List.partition (fun (_, f) -> match f with Operation _ -> true | _ -> false) trans_fields in
    let others = List.map fst others in
    let opersByName = ListExt.partitionAll fst3 (List.map fst opers) in
    let opersCollapsed = List.map (fun operByName ->
      let (name, presence, ty) =
        (List.fold_left (fun (_, _, acc) (name, presence, ty) ->
          match acc with
          | TBot -> (name, presence, unwrapArrow ty)
          | _ -> (name, presence, TIntersect (None, unwrapArrow ty, acc))) (P.empty, Maybe, TBot) operByName) in
      let wrappedTy = if ty = TPrim "Unsafe" then ty else wrapArrow SourceCell ty in
      (name, presence, wrappedTy)) opersByName in
    others @ opersCollapsed
  and trans_field tself queryInterfaceType field : (TypImpl.field * Full_idl_syntax.interfaceMember) list option = 
    match field with
    | Attribute (_, metas, readOnly, stringifier, typ, name) ->
      let returnField t = Some [((idToPat name, Present, t), field)] in
      if (isNoScript metas) then None
      else if (isPrivateBrowsingCheck metas) then returnField (TPrim "True")
      else if (isUnsafe metas) then returnField (TPrim "Unsafe")
      else if (isQueryInterfaceType metas) then returnField (queryInterfaceType 0)
      else (match isUseType metas with
      | Some (AttrArgList(_, [Full_idl_syntax.String ty])) -> returnField (parseType ty)
      | _ -> let t = trans_typ typ in 
           (* let t = match readOnly with *)
           (*   | NoReadOnly -> t *)
           (*   | ReadOnly -> TSource t in *)  (* TSource implies an extra indirection... *)
           Some [((idToPat name, Inherited, t), field)])
    | Operation (_, metas, stringifier, quals, retTyp, nameOpt, args) -> 
      let returnField typ = match nameOpt with
        | None -> None
        | Some name -> Some [((idToPat name, Inherited, typ), field)] in
      let transRetTyp = trans_typ retTyp in
      let adjoinThisRet argss ret =
        let allPossibleFunctionArgs = ListExt.product argss in
        (wrapArrow SourceCell
           (List.fold_left (fun acc args ->
             let arrowT = TArrow ((TThis tself) :: args, None, ret) in
             match acc with TBot -> arrowT | _ ->
               TIntersect (None, arrowT, acc)) TBot allPossibleFunctionArgs)) in
      if (isNoScript metas) then None
      else if (isUnsafe metas) then returnField (TPrim "Unsafe")
      else if (isQueryInterfaceType metas) then returnField (queryInterfaceType 0)
      else if (isGetter metas) then 
        let targs = trans_args tself queryInterfaceType args in
        let firstArg = List.hd (List.hd targs) in
        let getterFields = match firstArg with
          | TPrim "Num" -> let pat = P.parse 
                             Lexing.dummy_pos "(([0-9])*|(\"+Infinity\"|(\"-Infinity\"|\"NaN\")))" in
                           (pat, Present, transRetTyp) (* should be Maybe, but that gets annoying with Undefs... *)
          | TRegex r -> (r, Maybe, transRetTyp)
          | _ -> failwith "impossible" in
        let fakeAttr = Attribute(Lexing.dummy_pos, [], NoReadOnly, IsNormal, Any, Id.id_of_string "dummy") in
        (match nameOpt with None -> Some [(getterFields, fakeAttr)]
        | Some name -> Some [((idToPat name, Present, (adjoinThisRet targs transRetTyp)), field);
                             (getterFields, fakeAttr)])
      else (match isUseType metas with
      | Some (AttrArgList(_, [Full_idl_syntax.String ty])) -> 
        (match parseType ty with TRef (n, t) -> returnField (TSource (n, t)) | t -> returnField t)
      | _ -> match isQueryElementAtType metas with
        | Some (QueryElementAtType n) -> returnField (queryInterfaceType n)
        | _ -> begin 
          match List.rev args with
          | [] -> returnField (adjoinThisRet [] transRetTyp)
          | lastArg::revArgs ->
            let (_, lastMetas, lastTyp, _, _, _) = lastArg in
            let addArray metas t = if isArray metas then TApp(TId "Array", [t]) else t in
            let normalArrowArgs = 
              if (isRetval lastMetas)
              then (adjoinThisRet (trans_args tself queryInterfaceType (List.rev revArgs)) 
                      (addArray lastMetas (trans_typ lastTyp)))
              else (adjoinThisRet (trans_args tself queryInterfaceType args) transRetTyp) in
            returnField normalArrowArgs
        end)
    | ConstMember (_, metas, typ, id, value) -> 
      if (isNoScript metas)
      then None
      else Some [((idToPat id, Inherited, (* TSource *) (trans_typ typ)), field) (* can't do const correctly *)]
    | Stringifier (_, metas) -> None
  and trans_args tt queryInterfaceType args = 
    let helper (acc, skippedParams) ((_, _, _, _, id, _) as arg) =
      if (List.mem id skippedParams) then (acc, skippedParams)
      else
      match trans_arg tt queryInterfaceType arg with
      | t, Some skip -> (t::acc, skip::skippedParams)
      | t, None -> (t::acc, skippedParams) in
    (* let condense typs = *)
    (*   if (List.length (List.filter (fun t -> match t with TPrim _ | TRegex _ -> false | _ -> true) typs) > 1) *)
    (*   then typs *)
    (*   else match typs with *)
    (*   | [] -> [] *)
    (*   | [t] -> [t] *)
    (*   | hd::tl -> [List.fold_left (fun acc t -> TUnion(acc,t)) hd tl] in *)
    (* List.map condense *) (List.rev (fst (List.fold_left helper ([], []) args)))
  and trans_arg tt queryInterfaceType (direction, metas, typ, variadic, id, defaultOpt) = 
    match isUseType metas with
    | Some(AttrArgList(_, [Full_idl_syntax.String ty])) -> [parseType ty], None 
    | _ ->
    let findSizeOf metas = try Some (List.find (fun m -> match m with SizeOf _ -> true | _ -> false) metas)
      with Not_found -> None in
    let addSkip ts = match findSizeOf metas with
      | Some (SizeOf param) -> (ts, Some param)
      | _ -> (ts, None) in
    let addArray t = if isArray metas then TApp(TId "Array", [t]) else t in
    addSkip (match direction with
    | InParam -> 
      let addOptional ts = if isOptional metas then (TPrim "Undef") :: ts else ts in
      addOptional (match trans_typ typ with
      | (TId name) as t' -> (
        let name = Id.id_of_string name in
        match Hashtbl.find ifaceHash name with
        | Comp (_, Some funcFunc) -> [wrapArrow SourceCell (funcFunc tt); addArray t']
        | Comp (_, None) -> [addArray t']
        | Raw (_, false) -> [addArray t']
        | Raw (def, true) -> 
          ignore (trans_interface tt (fun _ -> queryInterfaceType) name);
          match Hashtbl.find ifaceHash name with
          | Comp (_, Some funcFunc) -> [wrapArrow SourceCell (funcFunc tt); addArray t']
          | Comp (_, None) -> [addArray t']
          | Raw _ -> failwith "Impossible")
      | t -> [addArray t]
      )
    | OutParam -> [TApp(TPrim "Immutable", [TApp(TId "Outparam", [trans_typ typ])])]
    | InOutParam -> [TApp(TId "Outparam", [trans_typ typ])])
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
    | Object -> TRef (None, TObject (mk_obj_typ [] P.all))
    | Any -> TTop
    | Void -> TPrim "Undef"
    | Ques t -> TUnion (None, TPrim "Null", trans_typ t)
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


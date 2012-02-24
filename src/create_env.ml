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


let print_env defs = 
  let idToPat id = P.singleton (Id.string_of_id id) in
  let filterNone xs = (* 'a option list -> 'a list *)
    let rec helper xs acc = match xs with
      | [] -> List.rev acc
      | Some x :: xs -> helper xs (x :: acc)
      | None :: xs -> helper xs acc
    in helper xs [] in
  let isNoScript metas = List.exists (fun m -> m = NoScript || m = NotXPCOM) metas in
  let isScriptable metas = List.exists (fun m -> m = Scriptable) metas in
  let isRetval metas = List.exists (fun m -> m = Retval) metas in
  let rec trans_defs tt defs = 
    let interfaces = (filterNone (List.map (trans_def tt) (sort_defs defs))) in
    let componentsAndCID = build_components (sort_defs defs) in
    TSource (TObject (mk_obj_typ (interfaces @ componentsAndCID) P.all))
  and build_components defs = 
    let interfaceToIIDFieldAndFun def =
      match def with
      | Interface (_, metas, name, _, _, _) ->
        if (not (isScriptable metas))
        then None
        else let iidName = (Id.string_of_id name) ^ "IID" in
             Some (iidName,
                   (idToPat name, Present, TId iidName),
                   TArrow ([TId "nsIJSCID"; TId iidName], TId (Id.string_of_id name)))
      | _ -> None in
    let unzip3 abcs =
      let rec helper abcs aas bbs ccs =
        match abcs with
        | [] -> (List.rev aas, List.rev bbs, List.rev ccs)
        | (a,b,c) :: abcs' -> helper abcs' (a :: aas) (b :: bbs) (c :: ccs) in
      helper abcs [] [] [] in
    let (iids, fields, funs) = unzip3 (filterNone (List.map interfaceToIIDFieldAndFun defs))in
    let compInterface = TSource (TObject (mk_obj_typ fields P.empty)) in
    let compClasses = TSource (TObject (mk_obj_typ [P.all, Maybe, TId "nsIJSCID"] P.empty)) in
    let componentsType = 
      TSource (TObject (mk_obj_typ [(P.singleton "interfaces", Present, compInterface);
                                    (P.singleton "classes", Present, compClasses)] P.empty)) in
    let queryInterfaceType = match funs with
      | [] -> TBot (* absurd *)
      | [ty] -> ty
      | f::fs -> List.fold_left (fun f acc -> TIntersect (f, acc)) f fs in
    let jscidType = 
      TSource (TObject (mk_obj_typ 
                          [(P.singleton "getService", Present, queryInterfaceType);
                           (P.singleton "createInstance", Present, queryInterfaceType)]
                          P.empty)) in
    [(P.singleton "Components", Present, componentsType);
     (P.singleton "nsIJSCID", Present, jscidType)]
  and trans_def tt def = match def with
    | Module (_, metas, name, defs) -> Some (idToPat name, Present, trans_defs tt defs)
    | Typedef _ -> None
    | Interface (_, metas, name, parent, members, isCallback) -> 
      let transfields = trans_fields (TId (Id.string_of_id name)) members in
      let ifaceTyp = match parent with
        | None -> TObject (mk_obj_typ transfields P.all)
        | Some pName -> match pName with
          | RelativeName [id] -> 
            TObject (mk_obj_typ ((proto_pat, Present, TId (Id.string_of_id id))
                                 :: transfields) P.all)
          | _ -> TObject (mk_obj_typ transfields P.all) (* absurd, won't happen *)
      in
      Some (idToPat name, Present, TSource ifaceTyp)
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
      if (isNoScript metas)
      then None
      else let t = trans_typ tt typ in 
           let t = match readOnly with
             | NoReadOnly -> t
             | ReadOnly -> TSource t in
           Some (idToPat name, Present, t)
    | Operation (_, metas, stringifier, quals, typ, nameOpt, args) -> 
      if (isNoScript metas)
      then None
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
      TObject (mk_obj_typ [(P.singleton "value", Present, TSink (trans_typ tt typ))] P.empty)
    | InOutParam -> 
      TObject (mk_obj_typ [(P.singleton "value", Present, (trans_typ tt typ))] P.empty)
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
    | Double -> TPrim Num
    | Boolean -> TUnion ((TPrim True), (TPrim False))
    | DOMString -> TId "DOMString"
    | Date -> TId "Date"
    | Object -> TObject (mk_obj_typ [] P.all)
    | Any -> TTop
    | Void -> TPrim Undef
    | Ques t -> TUnion ((TPrim Null), (trans_typ tt t))
    | Native s -> TId s
    | Name n -> begin match n with
      | RelativeName [id] -> TId (Id.string_of_id id)
      | _ -> TId "unknown"
    end
    | Array _
    | Sequence _ -> TId "unknown"
  in Pretty.typ (trans_defs TBot defs) std_formatter; Format.print_flush ()

  (* let namedBraces name items = *)
  (*   vert [horz [name; text "{"]; *)
  (*         horz [text " "; vert items]; *)
  (*         horz [text "}"]] in *)
  (* let rec print_defs defs = (List.map (fun d -> squish [print_def d; text ";\n"]) (sort_defs defs)) *)
  (* and print_def def = match def with *)
  (*   | Module(p, metas, id, defs) ->  *)
  (*     namedBraces (vert [print_metas metas; *)
  (*                        horz [text "Module"; print_id id]])  *)
  (*       (print_defs defs) *)
  (*   | Typedef(p, metas, typ, id) ->  *)
  (*     vert [print_metas metas; *)
  (*           horz [text "typedef"; print_typ typ; print_id id]] *)
  (*   | Interface(p, metas, id, parent, mems, isCallback) ->  *)
  (*     namedBraces (vert [print_metas metas; *)
  (*                        horz  *)
  (*                          [(match isCallback with *)
  (*                          | IsCallbackInterface -> text "callback" *)
  (*                          | _ -> empty); *)
  (*                           text "interface"; print_id id; print_parent parent]]) *)
  (*       (print_mems mems) *)
  (*   | ForwardInterface(p, metas, id) ->  *)
  (*     vert [print_metas metas; *)
  (*           horz [text "interface"; print_id id]] *)
  (*   | Exception(p, metas, id, parent, mems) ->  *)
  (*     namedBraces (vert [print_metas metas; *)
  (*                        horz [text "exception"; print_id id; print_parent parent]]) *)
  (*       (print_mems mems) *)
  (*   | Implements(p, metas, id, parent) -> *)
  (*     vert [print_metas metas; *)
  (*           horz [print_id id; text "implements"; print_id parent]] *)
  (*   | Const(p, metas, typ, id, value) ->  *)
  (*     vert [print_metas metas; *)
  (*           horz [text "const"; print_typ typ; print_id id; text "="; print_exp value]] *)
  (*   | Dictionary(p, metas, id, parent, mems) ->  *)
  (*     namedBraces (vert [print_metas metas; *)
  (*                        horz [text "dictionary"; print_id id; print_parent parent]]) *)
  (*       (print_mems mems) *)
  (*   | PartialInterface(p, metas, id, mems) ->  *)
  (*     namedBraces (vert [print_metas metas; *)
  (*                        horz [text "partial interface"; print_id id]]) *)
  (*       (print_mems mems) *)
  (*   | Include(p, metas, file) ->  *)
  (*     vert [print_metas metas; *)
  (*           squish [text "#include \""; text file; text "\""]] *)
  (*   | Callback(p, metas, id, args, ret) ->   *)
  (*     vert [print_metas metas; *)
  (*           horz [text "callback"; print_typ ret; print_id id; parens(listOf (List.map print_arg args))]] *)
  (*   | Enum(p, metas, id, enums) ->  *)
  (*     namedBraces (vert [print_metas metas; *)
  (*                        horz [text "enum"; print_id id]]) *)
  (*       (List.map text enums) *)
  (* and print_parent parent = match parent with *)
  (*   | None -> empty; *)
  (*   | Some id -> horz [text ":"; print_scoped id] *)
  (* and print_scoped name = match name with *)
  (*   | RelativeName ids -> squish (intersperse (text "::") (List.map print_id ids)) *)
  (*   | AbsoluteName ids -> squish [text "::"; (squish (intersperse (text "::") (List.map print_id ids)))] *)
  (* and print_id ident = text (Id.string_of_id ident) *)
  (* and print_metas metas = match metas with *)
  (*   | [] -> empty; *)
  (*   | _ -> brackets (listOf (List.map print_meta metas)) *)
  (* and listOf lst = squish (intersperse (text ", ") lst) *)
  (* and print_meta meta = match meta with *)
  (*   | NoInterfaceObject -> text "NoInterfaceObject" *)
  (*   | OverrideBuiltins -> text "OverrideBuiltins" *)
  (*   | PutForwards id -> squish [text "PutForwards="; print_id id] *)
  (*   | NamedConstructor (id, args) -> squish [text "NamedConstructor="; print_id id;  *)
  (*                                            parens (listOf (List.map print_arg args))] *)
  (*   | Constructor args -> squish [text "Constructor"; *)
  (*                                 parens (listOf (List.map print_arg args))] *)
  (*   | SizeOf id -> horz [squish [text "size_of"; parens (print_id id)]] *)
  (*   | ReplaceableNamedProperties -> text "ReplaceableNamedProperties" *)
  (*   | Unforgeable -> text "Unforgeable" *)
  (*   | Replaceable -> text "Replaceable" *)
  (*   | MCallback -> text "Callback (deprecated)" *)
  (*   | MCallbackFunctionOnly -> text "Callback=FunctionOnly (deprecated)" *)
  (*   | TreatNullAs t -> squish [text "TreatNullAs="; print_typ t] *)
  (*   | AllowAny -> text "AllowAny" *)
  (*   | NoScript -> text "noscript" *)
  (*   | Optional -> text "optional" *)
  (*   | OptionalArgc -> text "optional_argc" *)
  (*   | Clamp -> text "Clamp" *)
  (*   | Scriptable -> text "Scriptable" *)
  (*   | Uuid uuid -> squish [text "uuid"; parens (text uuid)] *)
  (*   | ImplicitJSContext -> text "implicit_jscontext" *)
  (*   | AttrNoArgs id -> print_id id *)
  (*   | AttrArgList (id, args) -> squish [print_id id; parens (listOf (List.map print_exp args))] *)
  (*   | AttrNamedIdent(name,id) -> squish [print_id name; text "="; print_id id] *)
  (*   | AttrNamedArgList (name, id, args) -> squish [print_id name; text "="; *)
  (*                                                  print_id id; parens (listOf (List.map print_exp args))] *)
  (* and print_unsigned u = match u with *)
  (*   | Unsigned -> text "unsigned" *)
  (*   | NoUnsigned -> empty *)
  (* and print_typ typ = match typ with *)
  (*   | Short u -> horz [print_unsigned u; text "short"] *)
  (*   | Long u -> horz [print_unsigned u; text "long"] *)
  (*   | LongLong u -> horz [print_unsigned u; text "long long"] *)
  (*   | Boolean -> text "boolean" *)
  (*   | Byte -> text "byte" *)
  (*   | Octet -> text "octet" *)
  (*   | Float -> text "float" *)
  (*   | Double -> text "double" *)
  (*   | DOMString -> text "DOMString" *)
  (*   | Date -> text "Date" *)
  (*   | Any -> text "any" *)
  (*   | Object -> text "object" *)
  (*   | Name n -> print_scoped n *)
  (*   | Void -> text "void" *)
  (*   | Array t -> squish [print_typ t; text "[]"] *)
  (*   | Ques t -> squish [print_typ t; text "?"] *)
  (*   | Sequence t -> squish [text "sequence"; angles (print_typ t)] *)
  (*   | Native t -> squish[text "native"; parens (text t)] *)
  (* and print_mems mems = List.map (fun m -> squish [print_mem m; text ";"]) (sort_members mems) *)
  (* and sort_members mems = List.stable_sort (fun m1 m2 ->  *)
  (*   let order mem = match mem with *)
  (*     | ConstMember _ -> 0 *)
  (*     | Attribute _ -> 1 *)
  (*     | Operation _ -> 2 *)
  (*     | Stringifier _ -> 3 *)
  (*   in match m1, m2 with *)
  (*   | Attribute (_, _, _, _, _, id1), Attribute (_, _, _, _, _, id2) *)
  (*   | Operation (_, _, _, _, _, Some id1, _), Operation (_, _, _, _, _, Some id2, _) *)
  (*   | ConstMember (_, _, _, id1, _), ConstMember (_, _, _, id2, _) ->  *)
  (*     compare (Id.string_of_id id1) (Id.string_of_id id2) *)
  (*   | Operation (_, _, _, _, _, None, args1), Operation (_, _, _, _, _, None, args2) -> compare args1 args2 *)
  (*   | Operation (_, _, _, _, _, Some _, _), Operation (_, _, _, _, _, None, _) -> 1 *)
  (*   | Operation (_, _, _, _, _, None, _), Operation (_, _, _, _, _, Some _, _) -> -1 *)
  (*   | _, _ -> compare (order m1) (order m2) *)
  (* ) mems *)
  (* and print_mem mem = match mem with *)
  (*   | Attribute(p, metas, readOnly, stringifier, typ, id) -> *)
  (*     vert [print_metas metas; *)
  (*           horz *)
  (*             [(match readOnly with *)
  (*              | ReadOnly -> text "readonly" *)
  (*              | _ -> text "        "); *)
  (*              (match stringifier with *)
  (*              | IsStringifier -> text "stringifier" *)
  (*              | _ -> empty); *)
  (*              print_typ typ; *)
  (*              print_id id]] *)
  (*   | Operation(p, metas, stringifier, quals, ret, name, args) -> *)
  (*     vert [print_metas metas; *)
  (*           horz *)
  (*             [(match stringifier with *)
  (*              | IsStringifier -> text "stringifier" *)
  (*              | _ -> empty); *)
  (*              (if quals.getter then text "getter" else empty); *)
  (*              (if quals.setter then text "setter" else empty); *)
  (*              (if quals.creator then text "creator" else empty); *)
  (*              (if quals.deleter then text "deleter" else empty); *)
  (*              (if quals.legacyCaller then text "legacyCaller" else empty); *)
  (*              (if quals.static then text "static" else empty); *)
  (*              squish  *)
  (*                [print_typ ret;  *)
  (*                 (match name with *)
  (*                 | None -> empty *)
  (*                 | Some id -> squish [text " "; print_id id]); *)
  (*                 parens (listOf (List.map print_arg args))]]] *)
  (*   | ConstMember(p, metas, typ, id, value) -> *)
  (*     vert [print_metas metas; *)
  (*           horz [text "const"; print_typ typ; print_id id; text "="; print_exp value]] *)
  (*   | Stringifier(p, metas) -> *)
  (*     vert [print_metas metas; text "stringifier;"] *)
  (* and print_exp e =  *)
  (*   let prec e = match e with *)
  (*     | UnOp _ -> 10 *)
  (*     | BinOp(_, Times, _) *)
  (*     | BinOp(_, Divide, _) *)
  (*     | BinOp(_, Mod, _) -> 6 *)
  (*     | BinOp(_, Plus, _) *)
  (*     | BinOp(_, Minus, _) -> 5 *)
  (*     | BinOp(_, ShLeft, _) *)
  (*     | BinOp(_, ShRight, _) -> 4 *)
  (*     | BinOp(_, And, _) -> 3 *)
  (*     | BinOp(_, Xor, _) -> 2 *)
  (*     | BinOp(_, Or, _) -> 1 *)
  (*     | _ -> 20 in *)
  (*   let binOp b = match b with  *)
  (*     | Plus -> text "+" *)
  (*     | Minus -> text "-" *)
  (*     | Times -> text "*" *)
  (*     | Divide -> text "/" *)
  (*     | Mod -> text "%" *)
  (*     | ShLeft -> text "<<" *)
  (*     | ShRight -> text ">>" *)
  (*     | And -> text "&" *)
  (*     | Xor -> text "^" *)
  (*     | Or -> text "|" in *)
  (*   let unOp o = match o with *)
  (*     | UPlus -> text "+" *)
  (*     | UMinus -> text "-" *)
  (*     | UTilde -> text "~" in *)
  (*   let rec helper e outerPrec = *)
  (*     let ePrec = prec e in *)
  (*     let parens = if (ePrec <= outerPrec) then parens else (fun x -> x) in *)
  (*     parens (match e with *)
  (*     | BinOp(l, o, r) -> horz [helper l ePrec; binOp o; helper r ePrec] *)
  (*     | UnOp(o, e) -> horz[unOp o; helper e ePrec] *)
  (*     | Ident n -> print_scoped n *)
  (*     | IntLit i -> text (Int64.to_string i) *)
  (*     | FloatLit f -> text (string_of_float f) *)
  (*     | String s -> squish[text "\""; text s; text "\""] *)
  (*     | Bool b -> if b then text "true" else text "false") in *)
  (*   helper e 0 *)
  (* and print_arg (inout, metas, typ, variadic, id, def) = *)
  (*   horz [print_metas metas; *)
  (*         (match inout with *)
  (*         | InParam -> text "in" *)
  (*         | OutParam -> text "out" *)
  (*         | InOutParam -> text "inout"); *)
  (*         print_typ typ; *)
  (*         (match variadic with *)
  (*         | Single -> empty *)
  (*         | Variadic -> text "..."); *)
  (*         (match def with *)
  (*         | None -> print_id id *)
  (*         | Some e -> horz [print_id id; text "="; print_exp e]) *)
  (*        ] *)
  (* in vert (print_defs defs) std_formatter *)

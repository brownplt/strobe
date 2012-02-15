module F = Full_idl_syntax
module S = Idl_syntax

let rec filterNone (lst : 'a option list) : 'a list  = match lst with
  | [] -> []
  | None :: tl -> filterNone tl
  | Some s :: tl -> s :: filterNone tl 
let from_full (src : F.definition list) : S.definition list = 
  let rec translate_defs defs : S.definition list = filterNone (List.map translate_def defs)
  and translate_def def : S.definition option = 
    match def with
    | F.Module(p, _, id, defs) -> Some (S.Module(p, id, translate_defs defs))
    | F.Typedef(p, _, typ, id) -> Some (S.Typedef(p, translate_typ typ, id))
    | F.Interface(p, meta, id, parent, mems, _) -> 
      Some (S.Interface(p, id, translate_scoped_opt parent, translate_mems mems, translate_metas meta))
    | F.ForwardInterface(p, _, id) -> Some (S.ForwardInterface (p, id))
    | F.Exception(p, _, id, parent, mems) -> Some (S.Exception (p, id, translate_mems mems))
    | F.Implements(p, _, id, parent) -> Some (S.Implements(p, id, parent))
    | F.Const(p, _, typ, id, _) -> Some (S.Const (p, translate_typ typ, id))
    | F.Dictionary(p, _, id, _, mems) -> Some (S.Dictionary(p, id, translate_mems mems))
    | F.PartialInterface(p, _, id, mems) -> Some (S.PartialInterface(p, id, translate_mems mems))
    | _ -> None
  and translate_unsigned u : S.unsigned = 
    match u with
    | F.Unsigned -> S.Unsigned
    | F.NoUnsigned -> S.NoUnsigned
  and translate_scoped n : S.scopedName = 
    match n with
    | F.RelativeName ids -> S.RelativeName ids
    | F.AbsoluteName ids -> S.AbsoluteName ids
  and translate_scoped_opt n : S.scopedName option = 
    match n with
    | Some n -> Some (translate_scoped n)
    | None -> None
  and translate_typ typ : S.typ =
    match typ with
    | F.Short u -> S.Short (translate_unsigned u)
    | F.Long u -> S.Long (translate_unsigned u)
    | F.LongLong u -> S.LongLong (translate_unsigned u)
    | F.Boolean -> S.Boolean
    | F.Byte -> S.Byte
    | F.Octet -> S.Octet
    | F.Float -> S.Float
    | F.Double -> S.Double
    | F.DOMString -> S.DOMString
    | F.Date -> S.Date
    | F.Any -> S.Any
    | F.Object -> S.Object
    | F.Name n -> S.Name (translate_scoped n)
    | F.Void -> S.Void
    | F.Array t -> S.Array (translate_typ t)
    | F.Ques t -> S.Ques (translate_typ t)
    | F.Sequence t -> S.Ques (translate_typ t)
  and translate_ro ro : S.readOnly =
    match ro with
    | F.ReadOnly -> S.ReadOnly
    | F.NoReadOnly -> S.NoReadOnly
  and translate_arg arg : S.typ =
    let (_, _, t, _, _, _) = arg in translate_typ t
  and translate_args args : S.typ list = List.map translate_arg args
  and translate_meta (m : F.meta) : S.meta option =
    match m with
    | F.NoInterfaceObject -> Some S.NoInterfaceObject
    | F.OverrideBuiltins -> Some S.OverrideBuiltins
    | F.PutForwards id -> Some (S.PutForwards id)
    | F.NamedConstructor(id, args) -> Some (S.NamedConstructor(id, translate_args args))
    | F.Constructor args -> Some (S.Constructor (translate_args args))
    | F.ReplaceableNamedProperties -> Some S.ReplaceableNamedProperties
    | F.Unforgeable -> Some S.Unforgeable
    | F.Replaceable -> Some S.Replaceable
    | F.MCallback -> Some S.Callback
    | F.MCallbackFunctionOnly -> Some S.CallbackFunctionOnly
    | F.TreatNullAs t -> Some (S.TreatNullAs (translate_typ t))
    | F.AllowAny -> Some S.AllowAny
    | F.Clamp -> Some S.Clamp
    | _ -> None
  and translate_metas (metas : F.meta list) : S.meta list = filterNone (List.map translate_meta metas)
  and translate_mem mem : S.interfaceMember list =
    match mem with
    | F.Attribute(p, _, ro, _, t, id) -> [S.Attribute(p, translate_ro ro, translate_typ t, id)]
    | F.Operation(_, _, _, _, _, None, _) -> []
    | F.Operation(p, _, _, quals, t, Some id, args) -> 
      if (quals.F.getter) 
      then [S.Getter(p, translate_typ t, translate_args args);
            S.Operation(p, translate_typ t, id, translate_args args)]
      else if (quals.F.setter)
      then [S.Setter(p, translate_typ t, translate_args args);
            S.Operation(p, translate_typ t, id, translate_args args)]
      else if (quals.F.creator)
      then [S.Creator(p, translate_typ t, translate_args args);
            S.Operation(p, translate_typ t, id, translate_args args)]
      else if (quals.F.deleter)
      then [S.Deletor(p, translate_typ t, translate_args args);
            S.Operation(p, translate_typ t, id, translate_args args)]
      else [S.Operation(p, translate_typ t, id, translate_args args)]
    | F.ConstMember(p, _, t, id, _) -> [S.ConstMember(p, translate_typ t, id)]
    | F.Stringifier _ -> []
  and translate_mems mems : S.interfaceMember list =
    List.concat (List.map translate_mem mems)
  in
  translate_defs src

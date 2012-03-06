open Full_idl_syntax
open Prelude
open Format
open FormatExt

let resolve_typedefs defs =
  let (tds, defs) = List.partition (fun d -> match d with Typedef _ -> true | _ -> false) defs in
  let typedefs =  IdMapExt.from_list
    (List.map (fun d -> match d with
    | Typedef(_, metas, t, id) -> (Id.string_of_id id, (metas, t))
    | _ -> failwith "absurd") tds) in
  let rec resolve_typ ty =
    match ty with
    | Sequence t -> Sequence (resolve_typ t)
    | Ques t -> Ques (resolve_typ t)
    | Array t -> Array (resolve_typ t)
    | Name (RelativeName [id]) ->
      (try
         let (metas, ret) = (IdMap.find (Id.string_of_id id) typedefs) in
         if ret != ty && not (List.exists (fun m -> m = AttrNoArgs(Id.id_of_string "final")) metas)
         then resolve_typ ret
         else ty
       with Not_found -> ty)
    | _ -> ty in
  let rec resolve_def def =
    match def with
    | Module (p, metas, id, defs) -> Module (p, metas, id, List.map resolve_def defs)
    | Typedef _ -> def
    | Interface(p, metas, id, parent, mems, callback) ->
      Interface (p, metas, id, parent, List.map resolve_mem mems, callback)
    | ForwardInterface _ -> def
    | Exception(p, metas, id, parent, mems) ->
      Exception(p, metas, id, parent, List.map resolve_mem mems)
    | Implements _ -> def
    | Const(p, metas, ty, id, value) -> Const(p, metas, resolve_typ ty, id, value)
    | Dictionary(p, metas, id, parent, mems) ->
      Dictionary(p, metas, id, parent, List.map resolve_mem mems)
    | PartialInterface(p, metas, id, mems) ->
      PartialInterface(p, metas, id, List.map resolve_mem mems)
    | Include _ -> def
    | Callback(p, metas, id, args, ret) ->
      Callback(p, metas, id, List.map resolve_arg args, resolve_typ ret)
    | Enum _ -> def
  and resolve_arg (io, metas, t, variadic, id, value) = (io, metas, resolve_typ t, variadic, id, value)
  and resolve_mem mem =
    match mem with
    | Attribute(p, metas, ro, s, t, id) -> Attribute(p, metas, ro, s, resolve_typ t, id)
    | Operation(p, metas, s, q, t, id, args) ->
      Operation(p, metas, s, q, resolve_typ t, id, List.map resolve_arg args)
    | ConstMember(p, metas, t, id, value) -> ConstMember(p, metas, resolve_typ t, id, value)
    | Stringifier _ -> mem
  in tds @ List.map resolve_def defs

let sanity_check defs =
  let rec check_def def =
    match def with
    | Module (_, _, _, defs) -> List.concat (List.map check_def defs)
    | Typedef _ -> []
    | Interface (_, metas, id, _, mems, _) ->
      if (List.exists (fun m -> m = Scriptable) metas)
      then List.concat (List.map (check_mem id) mems)
      else []
    | ForwardInterface _ -> []
    | Exception (_, metas, id, _, mems) ->
      if (List.exists (fun m -> m = Scriptable) metas)
      then List.concat (List.map (check_mem id) mems)
      else []
    | Implements _ -> []
    | Const(_, metas, ty, id, _) -> check_ty metas ty (Id.string_of_id id)
    | Dictionary(_, metas, id, _, mems) ->
      if (List.exists (fun m -> m = Scriptable) metas)
      then List.concat (List.map (check_mem id) mems)
      else []
    | PartialInterface (_, _, id, mems) -> List.concat (List.map (check_mem id) mems)
    | Include _ -> []
    | Callback (_, metas, id, args, ret) ->
      List.concat ((check_ty metas ret (Id.string_of_id id))::
                      (List.map (fun (_, _, ty, _, _, _) -> check_ty metas ty (Id.string_of_id id)) args))
    | Enum _ -> []
  and check_ty metas ty id = match ty with
    | Native _ -> if (List.exists (fun m -> m = NoScript || m = NotXPCOM) metas) then [] else [id]
    | Sequence t
    | Ques t
    | Array t  -> check_ty metas t id
    | _ -> []
  and check_mem parent mem =
    let parent = Id.string_of_id parent in
    match mem with
    | Attribute(_, metas, _, _, t, id) -> check_ty metas t (parent ^ "::" ^ Id.string_of_id id)
    | Operation(_, metas, _, _, t, id, args) ->
      let id = (match id with Some i -> Id.string_of_id i | None -> "<anonymous>") in
      List.concat ((check_ty metas t (parent ^ "::" ^ id))::
                      (List.map (fun (_, _, ty, _, _, _) -> check_ty metas ty (parent ^ "::" ^ id)) args))
    | ConstMember(_, metas, t, id, value) -> check_ty metas t (Id.string_of_id id)
    | Stringifier _ -> []
  in List.concat (List.map check_def defs)

let remove_dupes defs =
  let sorted = Print_full_idl.sort_defs defs in
  let rec remove_next defs = match defs with
    | d1::((d2::rest) as tail) ->
      let equals = begin match d1, d2 with
      | Include(_, _, f1), Include(_, _, f2) -> f1 = f2
      | ForwardInterface(_, _, id1), ForwardInterface(_, _, id2) -> id1 = id2
      | _ -> false
      end in
      if equals then remove_next tail else d1::remove_next tail
    | _ -> defs
  in remove_next sorted

let resolve_partials defs =
  (* Hashtbl from PartialInterface names (String) to a list ('a list) of members of
     all PartialInterfaces with that name *)
  let partials = Hashtbl.create (List.length defs) in
  let rec get_partials defs = match defs with
    | PartialInterface(_,_,name,mems)::tail ->
      Hashtbl.add partials name mems;
      get_partials tail
    | head::tail -> get_partials tail
    | _ -> () in
  (* Where acc is the modified list of defs *)
  let rec resolve defs acc = match defs with
    | Interface(p,m,name,pn,imems,cb)::tail ->
      resolve tail (Interface(p,m,name,pn,
                              imems @ List.concat(Hashtbl.find_all partials name),cb)::acc)
    | head::tail -> resolve tail (head::acc)
    | [] -> acc in
  (* Ugly to call get_partials like this? MUTATION MUST DIEEEEE *)
  get_partials defs;
  List.rev (resolve defs [])

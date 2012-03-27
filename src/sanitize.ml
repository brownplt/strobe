open Full_idl_syntax
open Prelude
open Format
open FormatExt

let resolve_typedefs defs =
  let finalMeta = AttrNoArgs(Id.id_of_string "final") in
  let (tds, defs) = List.partition (fun d -> match d with Typedef _ -> true | _ -> false) defs in
  let (fwds, defs) = List.partition (fun d -> match d with ForwardInterface _ -> true | _ -> false) defs in
  let unknownFwds = ListExt.filter_map (fun f -> match f with
    | ForwardInterface (_, _, id) ->
      if (not (List.exists (fun d -> match d with
      | Interface(_, _, id', _, _, _) -> id = id'
      | _ -> false) defs))
      then Some (Id.string_of_id id, ([finalMeta], Any))
      else None
    | _ -> None) fwds in
  let typedefs =
    (List.map (fun d -> match d with
    | Typedef(_, metas, t, id) ->
      begin match t with
      | Native _ ->
        let findStrings strs =
          List.exists (fun s -> List.exists (fun m -> m = AttrNoArgs(Id.id_of_string s)) metas) strs in
        if (findStrings ["domstring"; "utf8string"; "cstring"; "string"])
        then (Id.string_of_id id, (finalMeta::metas, DOMString))
        else (Id.string_of_id id, (finalMeta::metas, Any))
      | _ -> (Id.string_of_id id, (metas, t))
      end
    | _ -> failwith "absurd") tds) in
  let typedefs = IdMapExt.from_list (typedefs @ unknownFwds) in
  let rec resolve_typ ty =
    match ty with
    | Sequence t -> Sequence (resolve_typ t)
    | Ques t -> Ques (resolve_typ t)
    | Array t -> Array (resolve_typ t)
    | Name (RelativeName [id]) ->
      (try
         let (metas, ret) = (IdMap.find (Id.string_of_id id) typedefs) in
         if ret != ty && not (List.exists (fun m -> m = finalMeta) metas)
         then resolve_typ ret
         else ret
       with Not_found -> ty)
    | DOMString ->
      (try
         let (metas, ret) = (IdMap.find "DOMString" typedefs) in
         if ret != ty && not (List.exists (fun m -> m = finalMeta) metas)
         then resolve_typ ret
         else ret
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
    | Implements(_, _, id1, id2) -> def
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
  let sorted = (* Print_full_idl.sort_defs *) defs in
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
  let partials = List.fold_left (fun pmap d -> match d with
    | PartialInterface(_,_,id,_) as p ->
      let sid = (Id.string_of_id id) in
      if (IdMap.mem sid  pmap) then
        IdMap.add sid (p::(IdMap.find sid pmap)) pmap
      else IdMap.add sid [p] pmap
    | _ -> pmap)
    IdMap.empty defs in
  (* Fold over all defs, merging each interface *)
  let rec merge_ifaces defs = List.fold_left (fun res_defs d -> match d with
    | Interface _ as i -> (merge_iface i)::res_defs
    | PartialInterface _ -> res_defs (* Drop out of it's a partial *)
    | Implements _ -> d::res_defs
    | _ -> res_defs)
    [] defs
  (* Merge an interface with its partials *)
  and merge_iface i = match i with
    | Interface(pos,metas,id,sn,mems,icb) as i ->
      let sid = Id.string_of_id id in
      (* If Interface has one or more PartialInterfaces *)
      if (IdMap.mem sid partials) then
        (* pmems is a lol *)
        let lo_pmems = List.fold_left (fun pmems p -> match p with
          | PartialInterface(_,_,_,m) -> m::pmems
          | _ -> raise(invalid_arg "must take a PartialInterface"))
          [] (IdMap.find sid partials) in
        (* This is a set (dupes removed) *)
        let pmetas =  List.fold_left (fun pmetas p -> match p with
          | PartialInterface(_,m,_,_) -> merge_metas m pmetas
          | _ -> raise(invalid_arg "must take a PartialInterface"))
          [] (IdMap.find sid partials) in
        (* Interface fully merged with all of its PartialInterfaces *)
        Interface(pos,(merge_metas metas pmetas), id, sn, (merge_members mems lo_pmems), icb)
      (* Otherwise, this Interface i has no PartialInterfaces *)
      else i
    | _ -> raise(invalid_arg "merge_iface must take an Interface")
  (* Merges two lists of members. imems is the list of interface members,
     and lo_pmems is the lol of all members of all corresponding partial interfaces.
     In the event of a merge, the resulting member will retain the pos value of
     the interface member *)
  and merge_members imems lo_pmems = List.fold_left (fun merged_mems pmems ->
    (* Fold over list of partial members pmems, merging each pm into imems *)
    List.fold_left (fun merged_mems pm ->
      (* merges a partial member pm into the list of interface members imems *)
      let merge_member imems pm =
        let rec helper acc imems = match imems with
          | im::rest -> (* im is the interface member *)
            begin match (im,pm) with
              | (Attribute(ip,imetas,iro,is,it,iid), Attribute(_,pmetas,pro,ps,pt,pid)) ->
                if (iid = pid & iro = pro & is = ps & it = pt) then
                  Attribute(ip,(merge_metas imetas pmetas),iro,is,it,iid)::(acc @ rest)
                else helper (im::acc) rest
              | (Operation(ip,imetas,is,iq,it,iid,ia), Operation(_,pmetas,ps,pq,pt,pid,pa)) ->
                if (iid = pid & is = ps & iq = pq & it = pt & ia = pa) then
                  Operation(ip,(merge_metas imetas pmetas),is,iq,it,iid,ia)::(acc @ rest)
                else helper (im::acc) rest
              | (ConstMember(ip,imetas,it,iid,ie), ConstMember(_,pmetas,pt,pid,pe)) ->
                if (iid = pid & it = pt & ie = pe) then
                  ConstMember(ip,(merge_metas imetas pmetas),it,iid,ie)::(acc @ rest)
                else helper (im::acc) rest
              (* This is a guess at how to resolve Stringifiers.. would need to see examples
                 to be certain *)
              | (Stringifier(ip,imetas), Stringifier(_,pmetas)) ->
                Stringifier(ip,(merge_metas imetas pmetas))::(acc @ rest)
              | (_,_) -> helper (im::acc) rest
            end
          (* Still haven't found a match, so tack on the the end *)
          | [] -> pm::acc in
        helper [] imems in
      merge_member merged_mems pm)
      merged_mems pmems)
    imems lo_pmems
  (* Assumes that each list of metas is a set *)
  and merge_metas metas1 metas2 = List.fold_left (fun merged_metas m ->
    if (List.exists (fun m2 -> m = m2) metas2) then merged_metas
    else m::merged_metas)
    metas2 metas1 in
  (* Resolve *)
  merge_ifaces defs

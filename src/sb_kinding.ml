open Prelude
open Typedjs_syntax
open TypImpl

exception Kind_error of string

type kind_env = kind IdMap.t

let valid_prims = ref (IdSetExt.from_list [ ])

let list_prims () = IdSetExt.to_list !valid_prims

let new_prim_typ (s : string) : unit =
  if IdSet.mem s !valid_prims then
    raise (Kind_error (s ^ " is already defined as a primitive type"))
  else (
    (* Printf.printf "Adding prim %s\n" s; *)
    valid_prims := IdSet.add s !valid_prims)

let kind_mismatch typ calculated_kind expected_kind = 
  raise 
    (Kind_error 
       (sprintf "Expected kind %s, but got kind %s for type:\n%s"
          (string_of_kind expected_kind)
          (string_of_kind calculated_kind)
          (string_of_typ typ)))


let rec kind_check (env : kind_env) (recIds : id list) (typ : typ) : kind = match typ with
  | TTop
  | TBot
  | TRegex _ -> KStar
  | TUninit t -> begin match !t with
    | None -> kind_check env recIds (TPrim "Undef")
    | Some t -> kind_check env recIds t
  end
  | TPrim s -> 
    if IdSet.mem s !valid_prims then 
      KStar
    else
      raise (Kind_error (s ^ " is not a primitive type"))
  | TUnion (_, t1, t2)
  | TIntersect (_, t1, t2) ->
    begin match kind_check env recIds t1, kind_check env recIds t2 with
    | KStar, KStar -> KStar
    | k1, KStar -> kind_mismatch t1 k1 KStar
    | _, k2 -> kind_mismatch t2 k2 KStar
    end
  | TThis t -> kind_check env recIds t
  | TRef (_, t)
  | TSource (_, t)
  | TSink (_, t) ->
    begin match kind_check env recIds t with
    | KStar -> KStar
    | k -> kind_mismatch t k KStar
    end
  | TArrow (arg_typs, varargs, result_typ) ->
    let assert_kind t = match kind_check env recIds t with
      | KStar -> ()
      | k -> kind_mismatch t k KStar in
    List.iter assert_kind (result_typ :: arg_typs);
    (match varargs with None -> () | Some v -> assert_kind v);
    KStar
  | TObject o ->
    List.iter (assert_fld_kind env recIds) (fields o);
    KStar
  | TWith(t, flds) ->
    ignore (kind_check env recIds t);
    List.iter (assert_fld_kind env recIds) (fields flds);
    KStar
  | TId x -> 
    begin 
      try IdMap.find x env
      with Not_found ->
        if (not (List.mem x recIds)) then
          (* let strfmt = Format.str_formatter in *)
          (* let envText = (IdMap.iter (fun id k ->  *)
          (*   FormatExt.horz [FormatExt.text id; FormatExt.text "="; Pretty.kind k] strfmt; *)
          (*   Format.pp_print_newline strfmt () *)
          (* ) env); Format.flush_str_formatter() in *)
          let s = (sprintf "type variable %s is unbound in env" x (* envText *)) in
          (* Printf.printf "%s" s; print_newline(); *)
          raise (Kind_error s)
        else KStar
    end
  | TForall (_, x, t1, t2) ->
    begin match kind_check env recIds t1, kind_check (IdMap.add x KStar env) recIds t2 with
    | KStar, KStar -> KStar
    | k1, KStar -> kind_mismatch t1 k1 KStar
    | _, k2 -> kind_mismatch t2 k2 KStar
    end
  | TRec (_, x, t) ->
    begin match kind_check (IdMap.add x KStar env) recIds t with
    | KStar -> KStar
    | k -> kind_mismatch t k KStar
    end
  | TLambda (_, args, t) ->
    let env' = fold_right (fun (x, k) env -> IdMap.add x k env) args env in
    KArrow (List.map snd2 args, kind_check env' recIds t)
  | TFix (_, x, k, t) ->
    let k' = kind_check (IdMap.add x k env) recIds t in
    if  k' = k then k
    else kind_mismatch typ k' k
  | TApp (t_op, t_args) ->
    begin 
      let check k_arg t_arg = 
        let k_actual = kind_check env recIds t_arg in
        if k_arg = k_actual then
          ()
        else 
          kind_mismatch t_arg k_actual k_arg in
      match t_op with
      | TPrim ("Constructing" as p)
      | TPrim ("Mutable" as p) 
      | TPrim ("Immutable" as p) ->
        begin 
          try List.iter2 check [KStar] t_args
          with Invalid_argument _ -> raise (Kind_error (p ^ "<> expects one argument"))
        end;
        KStar
      | _ -> match kind_check env recIds t_op with
        | KArrow (k_args, k_result) ->
          begin 
            try
              List.iter2 check k_args t_args;
              k_result
            with Invalid_argument _ ->
              raise (Kind_error
                       (sprintf "operator expects %d args, given %d"
                          (List.length k_args) (List.length t_args)))
          end
        | KStar ->
          raise (Kind_error 
                   (sprintf "not a type operator:\n%s" (string_of_typ t_op)))
    end

and assert_fld_kind (env : kind_env) recIds (_, _, t) = match kind_check env recIds t with
  | KStar -> ()
  | k -> kind_mismatch t k KStar

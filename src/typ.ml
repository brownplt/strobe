open Prelude
open Sig

module L = ListExt

module Make (Pat : SET) : (TYP with module Pat = Pat) = struct
      
  let num_typ_errors = ref 0

  let error_on_mismatch = ref false

  let with_typ_exns thunk = 
    let prev = !error_on_mismatch in
    error_on_mismatch := true;
    let r = thunk () in
    error_on_mismatch := prev;
    r

  let get_num_typ_errors () = !num_typ_errors

  module P = Pat
  module Pat = Pat
  type pat = P.t

  type kind = 
    | KStar
    | KArrow of kind list * kind
  
  type presence = 
    | Inherited
    | Present
    | Maybe
  
  type typ = 
    | TPrim of string
    | TUnion of string option * typ * typ
    | TIntersect of string option * typ * typ
    | TArrow of typ list * typ option * typ (* args (including <this>), optional variadic arg, return typ *)
    | TThis of typ
    | TObject of obj_typ
    | TWith of typ * obj_typ
    | TRegex of pat
    | TRef of string option * typ
    | TSource of string option * typ
    | TSink of string option * typ
    | TTop
    | TBot
    | TForall of string option * id * typ * typ (** [TForall (a, s, t)] forall a <: s . t *)
    | TId of id
    | TRec of string option * id * typ 
    | TLambda of string option * (id * kind) list * typ (** type operator *)
    | TApp of typ * typ list (** type operator application *)
    | TFix of string option * id * kind * typ (** recursive type operators *)
    | TUninit of typ option ref (** type of not-yet initialized variables -- write once to the ref to update *)

  and obj_typ = { 
    fields : (pat * presence * typ) list;
    absent_pat : pat;
    cached_parent_typ : typ option option ref;
    cached_guard_pat : pat option ref;
    cached_cover_pat : pat option ref;
    cached_possible_cover_pat : pat option ref; (* pat Lazy.t *)
  }
      


  type typ_error_details =
    | TypKind of (typ -> kind -> string) * typ * kind
    | StringTyp of (string -> typ -> string) * string * typ
    | FixedString of string
    | String of (string -> string) * string
    | TypTyp of (typ -> typ -> string) * typ * typ
    | NumNum of (int -> int -> string) * int * int
    | Typ of (typ -> string) * typ
    | Pat of (pat -> string) * pat
    | PatPat of (pat -> pat -> string) * pat * pat
    | PatPatTyp of (pat -> pat -> typ -> string) * pat * pat * typ
    | PatTyp of (pat -> typ -> string) * pat * typ
    | TypTypTyp of (typ -> typ -> typ -> string) * typ * typ * typ


  exception Typ_error of Pos.t * typ_error_details

  let typ_error_details_to_string s = match s with
    | TypKind(s, t, k) -> s t k
    | StringTyp(s,m,t) -> s m t
    | FixedString s -> s
    | String(s, m) -> s m
    | TypTyp(s, t1, t2) -> s t1 t2
    | NumNum(s, d1, d2) -> s d1 d2
    | Typ(s, t) -> s t
    | Pat(s, p) -> s p
    | PatPat(s, p1, p2) -> s p1 p2
    | PatPatTyp(s, p1, p2, t) -> s p1 p2 t
    | PatTyp(s, p, t) -> s p t
    | TypTypTyp(s, t1, t2, t3) -> s t1 t2 t3
  let typ_mismatch p s = 
    if !error_on_mismatch then
      raise (Typ_error (p, s))
    else
      begin
        incr num_typ_errors;
        eprintf "type error at %s : %s\n" (Pos.toString p) (typ_error_details_to_string s)
      end

  type field = pat * presence * typ

  type typenv = (typ * kind) IdMap.t

  (* Necessary for equi-recursive subtyping. *)
  module TypPair = struct
    type t = typ * typ
    let compare = Pervasives.compare
  end
  module TPSet = Set.Make (TypPair)
  module TPSetExt = SetExt.Make (TPSet)


  let proto_str = "__proto__"
    
  let proto_pat = P.singleton proto_str
  

  module Pretty = struct

    open Format
    open FormatExt
      
    let useNames = ref true
    let rec kind k = match k with
      | KStar -> text "*"
      | KArrow (ks, k) -> 
  horz [horz (intersperse (text ",") (map pr_kind ks)); text "=>"; kind k]

    and pr_kind k = match k with
      | KArrow _ -> parens (kind k)
      | _ -> kind k
        
    let hnest n (p : printer) (fmt : formatter) : unit =
      pp_open_hvbox fmt n;
      p fmt;
      pp_close_box fmt ()

    let print_space fmt = pp_print_space fmt ()

    let hvert (p : printer list) (fmt : formatter) : unit =
      hnest 2 (squish (intersperse print_space p)) fmt

    let rec typ t = typ' false t
    and typ' horzOnly t = 
      let typ = typ' horzOnly in 
      let hnestOrHorz n = if horzOnly then horz else (fun ps -> hnest n (squish ps)) in
      let namedType name fmt = 
        if !useNames 
        then match name with None -> fmt | Some n -> text n 
        else match name with None -> horz [text "Unnamed"; fmt] | Some n -> horz [text "Named"; text n; fmt] in
      let namedRef name mut fmt = 
        if !useNames
        then match name with None -> fmt | Some n -> squish [text mut; text n] 
        else match name with None -> horz [text "Unnamed*"; fmt] | Some n -> horz [text "Named*"; text n; fmt] in
      match t with
      | TTop -> text "Any"
      | TBot -> text "DoesNotReturn"
      | TPrim p -> text ("@" ^ p)
      | TLambda (n, args, t) -> 
        let p (x, k) = horz [ text x; text "::"; kind k ] in
        namedType n (hvert [horz [text "Lambda"; horz (map p args); text "."]; typ t ])
      | TFix (n, x, k, t) -> 
        namedType n (hvert [horz [text "Fix"; text x; text "::"; kind k; text "."]; typ t ])
      | TApp (t, ts) ->
        (match ts with
        | [] -> horz [typ t; text "<>"]
        | _ -> parens (horz [typ t; angles (horz (intersperse (text ",") (map typ ts)))]))
      | TRegex pat -> text (P.pretty pat)
      | TUnion (n, t1, t2) ->
        namedType n (
        let rec collectUnions t = match t with
          | TUnion (_, t1, t2) -> 
            let (t1h, t1s) = collectUnions t1 in
            let (t2h, t2s) = collectUnions t2 in
            (t1h, t1s @ (t2h :: t2s))
          | _ -> (t, []) in
        let unions = collectUnions t in
        begin match unions with
        | (t1, [t2]) -> parens (hnestOrHorz 0 [squish [horz [typ t1; text "+"]]; 
                                            if horzOnly then typ t2 else horz[empty;typ t2]])
        | (t, ts) -> 
          if (List.length ts > 1200) then parens (horz[typ t;
                                                       squish[text "+ ...(";
                                                              int (List.length ts);
                                                              text " total fields)....."]]) else
            parens (hnest (-1) 
                      (squish (intersperse print_space 
                                 ((horz [empty; typ t]) :: List.map (fun t -> horz [text "+"; typ t]) ts))))
        end)
      | TIntersect (n, t1, t2) -> (* horz [typ t1; text "&"; typ t2] *)
        namedType n (
        let rec collectIntersections t = match t with
          | TIntersect (_, t1, t2) -> 
            let (t1h, t1s) = collectIntersections t1 in
            let (t2h, t2s) = collectIntersections t2 in
            (t1h, t1s @ (t2h :: t2s))
          | _ -> (t, []) in
        let intersections = collectIntersections t in
        begin match intersections with
        | (t1, [t2]) -> parens (hnest 0 (squish [squish [horz [typ t1; text "&"]]; 
                                                 if horzOnly then typ t2 else horz[empty;typ t2]]))
        | (t, ts) -> 
          if (List.length ts > 1200) then parens (horz[typ t;
                                                       squish[text "& ...(";
                                                              int (List.length ts);
                                                              text " total fields)....."]]) else
            parens (hnest (-1) 
                      (squish (intersperse print_space 
                                 ((horz [empty; typ t]) :: List.map (fun t -> horz [text "&"; typ t]) ts))))
        end)
      | TThis t -> squish [text "this"; parens (typ t)]
      | TArrow (tt::arg_typs, varargs, r_typ) ->
        let multiLine = horzOnly ||
          List.exists (fun at -> match at with 
            TArrow _ | TObject _ -> true | _ -> false) arg_typs in
        let rec pairOff ls = match ls with
          | [] -> []
          | [_] -> ls
          | a::b::ls -> horz [a;b] :: pairOff ls in
        let vararg = match varargs with
          | None -> []
          | Some t -> [horz[squish [parens(horz[typ' true t]); text "..."]]] in
        let argTexts = 
          (intersperse (text "*") 
             ((map (fun at -> begin match at with
             | TArrow _ -> parens (horz [typ' true at])
             | _ -> typ' true at 
             end) arg_typs) @ vararg)) in
        hnestOrHorz 0
          [ squish [brackets (typ tt); 
                    (if multiLine 
                     then vert (pairOff (text " " :: argTexts)) 
                     else horz (empty::argTexts))] ;
            horz [text "->"; typ r_typ ]]
      | TArrow (arg_typs, varargs, r_typ) ->
        let vararg = match varargs with
          | None -> []
          | Some t -> [horz[squish [parens(horz[typ' true t]); text "..."]]] in
        let argText = horz (intersperse (text "*") 
                              ((map (fun at -> begin match at with
                              | TArrow _ -> parens (horz [typ' true at])
                              | _ -> typ' true at 
                              end) arg_typs) @ vararg)) in
        hnestOrHorz 0 [ argText; horz [text "->"; typ r_typ ]]
      | TObject flds -> 
        let isFunctionObject fieldList = 
          let findField namePat =
            try
              let (_, _, typ) =
                List.find (fun (n, p, _) -> (p = Present && n = namePat)) fieldList in
              (true, Some typ)
            with Not_found -> (false, None) in
          let (hasProto, _) = findField proto_pat in
          let (hasCode, codeTyp) = findField (P.singleton "-*- code -*-") in
          let (hasPrototype, protoTyp) = findField (P.singleton "prototype") in
          let isSimplePrototype = match protoTyp with
            | Some (TId t) -> t = "Object" || t = "Any" || t = "Ext"
                                                || (String.length t > 3 && String.sub t 0 3 = "nsI")
            | _ -> false in
          if ((List.length fieldList) = 4 && hasProto && hasCode && hasPrototype && isSimplePrototype)
          then codeTyp
          else None in
        (match isFunctionObject (flds.fields) with
        | Some arrTyp -> horz [squish [text "{|"; typ' true arrTyp; text "|}"]]
        | None ->
          if (List.length flds.fields > 1200) then braces (squish[text "...(";
                                                                  int (List.length flds.fields);
                                                                  text " total fields)....."]) else
          let abs = horz [ text (P.pretty flds.absent_pat); text ": _" ] in
          braces (hnestOrHorz 0 (intersperse print_space (map pat (flds.fields) @ [abs])))
        )
      | TWith (t, flds) ->
        let abs = horz [ text (P.pretty flds.absent_pat); text ": _" ] in
        braces (hnestOrHorz 0 (typ t :: text " with" :: print_space :: 
                                 intersperse print_space (map pat (flds.fields) @ [abs])))
      | TRef (n, s) -> namedRef n "rw:" (horz [ text "Ref"; parens (typ s) ])
      | TSource (n, s) -> namedRef n "r:" (horz [ text "Src"; parens (typ s) ])
      | TSink (n, s) -> namedRef n "w:" (horz [ text "Snk"; parens (typ s) ])
      | TForall (n, x, s, t) -> 
        namedType n (hvert [ horz [text "forall"; text x; text "<:"; typ s; text "."]; typ t ])
      | TId x -> text x
      | TRec (n, x, t) -> namedType n (horz [ text "rec"; text x; text "."; typ t ])
      | TUninit t -> match !t with
        | None -> text "???"
        | Some t -> squish[text "?"; typ t; text "?"]
  
    and pat (k, pres, p) = 
          let pretty_pres = match pres with
            | Present -> text "!"
            | Maybe -> text "?"
            | Inherited -> text "^" in
          horz [ text (P.pretty k); squish [text ":"; pretty_pres]; typ p; text "," ]
  end

  let string_of_typ = FormatExt.to_string Pretty.typ
  let string_of_kind = FormatExt.to_string Pretty.kind

  let absent_pat ot = ot.absent_pat

  let mk_obj_typ (fs: field list) (absent_pat : pat): obj_typ = 
    { fields = fs;
      absent_pat = absent_pat;
      cached_parent_typ = ref None;
      cached_guard_pat = ref None;
      cached_possible_cover_pat = ref None; (* lazy (P.unions (L.map fst3 fs)); *)
      cached_cover_pat = ref None
    }
  
  let possible_field_cover_pat ot = match !(ot.cached_possible_cover_pat) with
    | Some p -> p
    | None -> let ret = P.unions (L.map fst3 ot.fields) in
              ot.cached_possible_cover_pat := Some ret;
              ret
  (* Lazy.force ot.cached_possible_cover_pat *)

  let cover_pat ot =  match !(ot.cached_cover_pat) with
    | None -> 
      let p = P.union (absent_pat ot) (possible_field_cover_pat ot) in
      ot.cached_cover_pat := Some p;
      p
    | Some p -> p

  let fields ot = ot.fields

  let rec free_typ_ids (typ : typ) : IdSet.t =
    let open IdSet in
    let open IdSetExt in
    match typ with
      | TTop
      | TBot
      | TPrim _ 
      | TRegex _ -> 
        empty
      | TId x -> 
        singleton x
      | TRef (_, t)
      | TSource (_, t)
      | TSink (_, t) ->
        free_typ_ids t
      | TIntersect (_, t1, t2)
      | TUnion (_, t1, t2) ->
        union (free_typ_ids t1) (free_typ_ids t2)
      | TArrow (ss, v, t) ->
        unions (free_typ_ids t :: (match v with None -> empty | Some v -> free_typ_ids v) :: (map free_typ_ids ss))
      | TThis t -> free_typ_ids t
      | TApp (t, ss) ->
        unions (free_typ_ids t :: (map free_typ_ids ss))
      | TObject o -> unions (L.map (fun (_, _, t) -> free_typ_ids t) o.fields)
      | TWith(t, flds) -> union (free_typ_ids t) (free_typ_ids (TObject flds))
      | TFix (_, x, _, t)
      | TRec (_, x, t) ->
        remove x (free_typ_ids t)
      | TForall (_, x, s, t) ->
        union (free_typ_ids s) (remove x (free_typ_ids t))
      | TLambda (_, xks, t) ->
        diff (free_typ_ids t) (from_list (map fst2 xks))
      | TUninit t -> match !t with 
        | None -> empty
        | Some ty -> free_typ_ids ty

  let rec typ_subst x s typ = match typ with
    | TPrim _ -> typ
    | TRegex _ -> typ
    | TId y -> if x = y then s else typ
    | TUnion (n, t1, t2) -> TUnion (n, typ_subst x s t1, typ_subst x s t2)
    | TIntersect (n, t1, t2) ->
      TIntersect (n, typ_subst x s t1, typ_subst x s t2)
    | TArrow (t2s, v, t3)  ->
      let opt_map f v = match v with None -> None | Some v -> Some (f v) in
      TArrow (map (typ_subst x s) t2s, opt_map (typ_subst x s) v, typ_subst x s t3)
    | TThis t -> TThis (typ_subst x s t)
    | TWith(t, flds) -> TWith(typ_subst x s t, 
                              mk_obj_typ (map (fun (n, p, t) -> (n, p, typ_subst x s t)) flds.fields)
                                flds.absent_pat)
    | TObject o ->
        TObject (mk_obj_typ (map (third3 (typ_subst x s)) o.fields) 
                            o.absent_pat)
    | TRef (n, t) -> TRef (n, typ_subst x s t)
    | TSource (n, t) -> TSource (n, typ_subst x s t)
    | TSink (n, t) -> TSink (n, typ_subst x s t)
    | TTop -> TTop
    | TBot -> TBot
    | TLambda (n, yks, t) ->
      let ys = IdSetExt.from_list (map fst2 yks) in
      if IdSet.mem x ys then
        typ
      else begin
        let fresh_var old = (* a, b, ... z, aa, bb, ..., zz, ... *)
          let rec try_ascii m n =
            let attempt = String.make m (Char.chr n) in
            if not (List.mem attempt old) then attempt
            else if (n < int_of_char 'z') then try_ascii m (n+1)
            else try_ascii (m+1) (Char.code 'a') in
          try_ascii 1 (Char.code 'a') in
        let free_t = free_typ_ids s in
        let (rev_new_yks, typ_substs) =
          List.fold_left (fun (new_yks, typ_substs) (y, k) ->
            if not (IdSet.mem y free_t) then ((y,k)::new_yks, typ_substs)
            else 
              let x = fresh_var ((IdMapExt.keys typ_substs) @ IdSetExt.to_list free_t) in
              ((x,k)::new_yks, IdMap.add y (TId x) typ_substs))
            ([], IdMap.empty) yks in
        let new_yks = List.rev rev_new_yks in
        let t' = IdMap.fold typ_subst typ_substs t in
        TLambda (n, new_yks, typ_subst x s t')
      end
    | TFix (n, y, k, t) ->
      if x = y then
        typ
      else begin
        assert (not (IdSet.mem y (free_typ_ids s)));
        TFix (n, y, k, typ_subst x s t)
      end
    | TForall (n, y, t1, t2) -> 
      if x = y then 
        typ
      else 
        TForall (n, y, typ_subst x s t1, typ_subst x s t2)
    | TRec (n, y, t) ->
      if x = y then
        typ
      else begin
        assert (not (IdSet.mem y (free_typ_ids s)));
        TRec (n, y, typ_subst x s t)
      end
    | TApp (t, ts) -> 
      TApp (typ_subst x s t, List.map (typ_subst x s) ts)
    | TUninit t -> match !t with
      | None -> typ
      | Some t -> typ_subst x s t

  let rec merge typ flds = match typ with
    | TUnion(n, t1, t2) -> TUnion (n, merge t1 flds, merge t2 flds)
    | TIntersect(n, t1, t2) -> TIntersect(n, merge t1 flds, merge t2 flds)
    | TObject o -> begin
      let unionPats = P.union (flds.absent_pat) (P.unions (map fst3 flds.fields)) in
      let restrict_field (n, p, t) =
        let n' = P.subtract n unionPats in
        if (P.is_empty n') then None
        else Some (n', p, t) in
      let oldFields = L.filter_map restrict_field o.fields in
      let newFields = oldFields @ flds.fields in
      let newAbsent = P.union (P.subtract o.absent_pat unionPats) (flds.absent_pat) in
      let newAbsent = if P.is_empty newAbsent then P.empty else newAbsent in
      let ret = TObject (mk_obj_typ newFields newAbsent) in
      ret
    end
    | TRec(n, id, t) -> TRec(n, id, merge t flds)
    | TRef (n, t) -> TRef (n, merge t flds)
    | TSource (n, t) -> TSource (n, merge t flds)
    | TSink (n, t) -> TSink (n, merge t flds)
    | TThis t -> TThis (merge t flds)
    | _ -> typ

  and apply_name n typ = match typ with
    | TUnion(None, t1, t2) -> TUnion(n, t1, t2)
    | TIntersect(None, t1, t2) -> TIntersect(n, t1, t2)
    | TForall(None, x, t, b) -> TForall(n, x, t, b)
    | TRec(None, x, t) -> TRec(n, x, t)
    | TLambda(None, ts, t) -> TLambda(n, ts, t)
    | TFix(None, x, k, t) -> TFix(n, x, k, t)
    | TRef(None, t) -> TRef(n, t)
    | TSource(None, t) -> TSource(n, t)
    | TSink(None, t) -> TSink(n, t)
    | _ -> typ

  and name_of typ =  match typ with
    | TUnion(name, _, _) -> name
    | TIntersect(name, _, _) -> name
    | TForall(name, _, _, _) -> name
    | TRec(name, _, _) -> name
    | TLambda(name, _, _) -> name
    | TFix(name, _, _, _) -> name
    | TRef(name, _) -> name
    | TSource(name, _) -> name
    | TSink(name, _) -> name
    | _ -> None

  and replace_name n typ = match typ with
    | TUnion(_, t1, t2) -> TUnion(n, t1, t2)
    | TIntersect(_, t1, t2) -> TIntersect(n, t1, t2)
    | TForall(_, x, t, b) -> TForall(n, x, t, b)
    | TRec(_, x, t) -> TRec(n, x, t)
    | TLambda(_, ts, t) -> TLambda(n, ts, t)
    | TFix(_, x, k, t) -> TFix(n, x, k, t)
    | TRef(_, t) -> TRef(n, t)
    | TSource(_, t) -> TSource(n, t)
    | TSink(_, t) -> TSink(n, t)
    | _ -> typ

  and expose_twith typenv typ = let expose_twith = expose_twith typenv in match typ with
    | TWith (t, flds) ->
      let t = match t with TId x -> (fst2 (IdMap.find x typenv)) | _ -> t in
      let flds' = mk_obj_typ (map (third3 expose_twith) flds.fields) flds.absent_pat in
      replace_name None (merge t flds')
    | TUnion(n, t1, t2) -> TUnion (n, expose_twith t1, expose_twith t2)
    | TIntersect(n, t1, t2) -> TIntersect(n, expose_twith t1, expose_twith t2)
    | TRec(n, id, t) -> TRec(n, id, expose_twith t)
    | TRef (n, t) -> TRef (n, expose_twith t)
    | TSource (n, t) -> TSource (n, expose_twith t)
    | TSink (n, t) -> TSink (n, expose_twith t)
    | TThis t -> TThis (expose_twith t)
    | _ -> typ

  and simpl_typ typenv typ = match typ with
    | TPrim _ 
    | TUnion _
    | TIntersect _
    | TRegex _
    | TArrow _
    | TRef _
    | TSource _
    | TSink _
    | TTop _
    | TBot _
    | TLambda _
    | TObject _
    | TId _
    | TThis _
    | TForall _ -> typ
    | TWith(t, flds) -> expose_twith typenv typ
    | TFix (n, x, k, t) -> apply_name n (simpl_typ typenv (typ_subst x typ t))
    | TRec (n, x, t) -> apply_name n (simpl_typ typenv (typ_subst x typ t))
    | TApp (t1, ts) -> 
      begin match expose typenv (simpl_typ typenv t1) with
      | TPrim "Constructing" -> List.hd ts
      | TPrim "Mutable" -> begin
        match ts with
        | [t] -> begin match expose typenv (simpl_typ typenv t) with
          | TRef (n, t) -> TRef (n, t)
          | TSource (n, t) -> TRef (n, t)
          | TSink (n, t) -> TRef (n, t)
          | _ -> raise (Invalid_argument "Expected a TRef, TSoruce or TSink argument to Mutable<T>")
        end
        | _ ->  raise (Invalid_argument "Expected one argument to Mutable<T>")
      end
      | TPrim "Immutable" -> begin
        match ts with
        | [t] -> begin match expose typenv (simpl_typ typenv t) with
          | TRef (n, t) -> TSource (n, t)
          | TSource (n, t) -> TSource (n, t)
          | TSink (n, t) -> TSource (n, t)
          | _ -> raise (Invalid_argument "Expected a TRef, TSoruce or TSink argument to Immutable<T>")
        end
        | _ ->  raise (Invalid_argument "Expected one argument to Mutable<T>")
      end
      | TLambda (n, args, u) -> 
        let name = match n with
          | None -> None
          | Some n ->
            let names = intersperse ", "
              (List.map (fun t -> match name_of t with Some n -> n | None -> string_of_typ t) ts) in
            Some (n ^ "<" ^ (List.fold_right (^) names ">")) in
        apply_name name
          (simpl_typ typenv
             (List.fold_right2 (* well-kinded, no need to check *)
                (fun (x, k) t2 u -> typ_subst x t2 u)
                args ts u))
      | func_t ->
        let msg = sprintf "ill-kinded type application in simpl_typ. Type is \
                           \n%s\ntype in function position is\n%s\n"
          (string_of_typ typ) (string_of_typ func_t) in
        raise (Invalid_argument msg)
    end
    | TUninit t -> match !t with
      | None -> typ
      | Some t -> simpl_typ typenv t

  and expose typenv typ = match typ with
    | TId x -> 
      (try 
        expose typenv (simpl_typ typenv (fst2 (IdMap.find x typenv)))
       with Not_found -> Printf.eprintf "Could not find type %s\n" x; raise Not_found)
    | TThis t -> TThis (expose typenv t)
    | _ -> typ

  let expose_arrow env typ = 
    let clear_id t = match t with TId x -> (fst2 (IdMap.find x env)) | _ -> t in
    let opt_clear_id t = match t with None -> None | Some t -> Some (clear_id t) in
    match typ with
    | TArrow(args, varargs, ret) -> TArrow(map clear_id args, opt_clear_id varargs, clear_id ret)
    | _ -> typ

  (** Decides if two types are syntactically equal. This helps subtyping. *)
  let rec simpl_equiv (typ1 : typ) (typ2 : typ) : bool = match (typ1, typ2) with
    | TTop, TTop
    | TBot, TBot ->
      true
    | TPrim p1, TPrim p2 ->
      p1 = p2
    | TIntersect (_, s1, s2), TIntersect (_, t1, t2)
    | TUnion (_, s1, s2), TUnion (_, t1, t2) -> 
      simpl_equiv s1 t1 && simpl_equiv s2 t2
    | TSource (_, s), TSource (_, t)
    | TSink (_, s), TSink (_, t)
    | TRef (_, s), TRef (_, t) ->
      simpl_equiv s t
    | TThis _, TThis _ -> true (* ASSUMING THAT THIS TYPES ARE EQUIVALENT *)
    | TApp (s1, s2s), TApp (t1, t2s) ->
      (* for well-kinded types, for_all2 should not signal an error *)
      simpl_equiv s1 t1 && List.for_all2 simpl_equiv s2s t2s
    | TId x, TId y ->
      x = y
    | TArrow (args1, v1, r1), TArrow (args2, v2, r2) ->
      List.length args1 = List.length args2
      && List.for_all2 simpl_equiv args1 args2
      && simpl_equiv r1 r2
      && (match v1, v2 with
      | None, None -> true
      | Some v1, Some v2 -> simpl_equiv v1 v2
      | _ -> false)
    | TRec (_, x, s), TRec (_, y, t) ->
      x = y && simpl_equiv s t
    | TForall (_, x, s1, s2), TForall (_, y, t1, t2) ->
      x = y && simpl_equiv s1 t1 && simpl_equiv s2 t2
    | TRegex pat1, TRegex pat2 ->
      P.is_equal pat1 pat2
    | TObject o1, TObject o2 ->
      let flds1 = fields o1 in
      let flds2 = fields o2 in
      List.length flds1 = List.length flds2
      && List.for_all2 simpl_equiv_fld flds1 flds2
      && P.is_equal o1.absent_pat o2.absent_pat
    | TFix (_, x1, k1, t1), TFix (_, x2, k2, t2) ->
      x1 = x2 && k1 = k2 && simpl_equiv t1 t2
    | TLambda (_, args1, t1), TLambda (_, args2, t2) ->
      args1 = args2 && simpl_equiv t1 t2
    | TUninit t1, TUninit t2 -> begin match !t1, !t2 with
      | None, None -> true
      | Some t1, Some t2 -> simpl_equiv t1 t2
      | _, _ -> false
    end
    | _, _ -> false

  and simpl_equiv_fld (pat1, pres1, t1) (pat2, pres2, t2) = 
    P.is_equal pat1 pat2 && pres1 = pres2 && simpl_equiv t1 t2

  let pat_env (env : typenv) : pat IdMap.t =
    let select_pat_bound (x, (t, _)) = match t with
      | TRegex p -> Some (x, p)
      | _ -> None in
    L.fold_right (fun (x,p) env -> IdMap.add x p env)
      (L.filter_map select_pat_bound (IdMap.bindings env))
      IdMap.empty

  exception Invalid_parent of string

  let rec parent_typ' env flds = match flds with
    | [] -> None
    | ((pat, pres, fld)::flds') -> 
       match P.is_subset (pat_env env) proto_pat pat with
      | true -> begin match pres with
        | Present -> begin match expose env (simpl_typ env fld) with
          | TPrim "Null" -> Some (TPrim "Null")
          | TSource (_, p)
          | TRef (_, p) -> Some (expose env (simpl_typ env p))
          | _ -> raise (Invalid_parent ("__proto__ is "^ (string_of_typ fld)))
          end
        | _ -> raise (Invalid_parent ("Looking for field " ^ (P.pretty pat) ^ " and __proto__ must be present or hidden"))
        end
      | false -> parent_typ' env flds'

  let rec parent_typ (env : typenv) typ = 
    match expose env (simpl_typ env typ) with
      | TObject ot -> begin match !(ot.cached_parent_typ) with
        | Some cached ->
          cached
        | None ->
          let computed = parent_typ' env ot.fields in
          ot.cached_parent_typ := Some computed;
          computed
      end
      | TUninit t -> begin match !t with
        | None -> raise (Invalid_argument "parent_typ expects TObject")
        | Some t -> parent_typ env t
      end
      | _ -> raise (Invalid_argument "parent_typ expects TObject")

  let rec calc_inherit_guard_pat (env : typenv) (t : typ) : pat =
    match t with
      | TObject ot ->
          begin match parent_typ env t with
            | None
            | Some (TPrim "Null") ->
              let f (pat, pres, _) = match pres with
                | Inherited
                | Present -> Some pat
                | Maybe _ -> None in
              P.unions (L.filter_map f ot.fields)
            | Some (TObject _) ->
              P.unions (ot.absent_pat::(L.map fst3 ot.fields))
            | Some pt ->
              raise (Invalid_argument 
                 ("invalid parent type in object type: " ^
               (string_of_typ pt)))
          end
      | TUninit t -> begin match !t with
        | None -> raise (Invalid_argument "calc_inherit_guard_pat expects TObject")
        | Some t -> calc_inherit_guard_pat env t
      end
      | t -> raise (Invalid_argument "expected TObject")

  let rec inherit_guard_pat env typ = match typ with
    | TObject ot -> begin match !(ot.cached_guard_pat) with
      | None -> let pat = calc_inherit_guard_pat env typ in
                ot.cached_guard_pat := Some pat;
                pat
      | Some pat -> pat
    end
    | TUninit t -> begin match !t with
      | None -> raise (Invalid_argument "inherit_guard_pat expects TObject, got TUninit None")
      | Some t -> inherit_guard_pat env t
    end
    | t -> 
      Printf.eprintf "ERROR: Expected object type, got:\n%s\n" (string_of_typ t);
      raise (Invalid_argument ("expected object type, got " ^
                                  (string_of_typ t)))

  let maybe_pats ot = 
    let f (pat, pres, _) acc = match pres with
      | Maybe -> P.union pat acc
      | _ -> acc in
    L.fold_right f ot.fields ot.absent_pat

  exception Not_subtype of typ_error_details

  let mismatched_typ_exn t1 t2 =
    (* Printf.printf "*** Not subtypes: %s </: %s\n" (string_of_typ t1) (string_of_typ t2); *)
    raise (Not_subtype 
       (TypTyp((fun t1 t2 -> sprintf " %s is not a subtype of %s"
         (string_of_typ t1) (string_of_typ t2)), t1, t2)))

  let rec simpl_lookup p (env : typenv) (t : typ) (pat : pat) : typ =
    (* TODO: it's okay to overlap with a maybe, but not a hidden field;
       inherit_guard_pat does not apply *)
    match t with
    | TObject ot -> 
      let guard_pat = P.unions (map fst3 ot.fields) in
      if P.is_subset (pat_env env) pat guard_pat then
        let sel (f_pat, _, f_prop) =
          if P.is_overlapped f_pat pat then Some f_prop
          else None in
        L.fold_right (fun s t -> typ_union env s t)
          (L.filter_map sel ot.fields)
          TBot
      else
        raise (Typ_error (p, FixedString "simpl_lookup: checking for a hidden or absent field"))
    | TUninit t -> begin match !t with
      | None -> raise (Invalid_argument "simpl_lookup expects TObject")
      | Some t -> simpl_lookup p env t pat
    end
    | _ -> raise (Typ_error (p, FixedString "simpl_lookup: object expected"))

  and inherits p (env : typenv) (orig_t : typ) (pat : pat) : typ =
    try
      let t = expose env (simpl_typ env orig_t) in
      if P.is_subset (pat_env env) pat (inherit_guard_pat env t) then
        begin match t with
        | TObject ot -> 
          let sel (f_pat, _, f_prop) =
            if P.is_overlapped f_pat pat then Some f_prop
            else None in
          L.fold_right (fun s t -> typ_union env s t)
            (L.filter_map sel ot.fields)
            (match parent_typ env t with
            | None
            | Some (TPrim "Null") -> TBot
            | Some parent_typ -> 
              if (simpl_equiv orig_t (expose env (simpl_typ env parent_typ))) 
              then orig_t
              else begin
                let check_parent_pat = (P.intersect pat (maybe_pats ot)) in
                (* Printf.printf "pat: %s\nmaybe_pat:%s\nintersect: %s\norig_t: %s\nparent_typ:%s\n\n" (P.pretty pat) (P.pretty (maybe_pats ot)) (P.pretty check_parent_pat) (string_of_typ orig_t)(string_of_typ (expose env (simpl_typ env parent_typ))); *)
                inherits p env parent_typ check_parent_pat
              end
            )
        | TUninit t -> begin match !t with
          | None -> raise (Invalid_argument "inherits expects TObject")
          | Some t -> inherits p env t pat
        end
        | _ -> failwith "lookup non-object"
        end
      else begin match parent_typ env t with
      | Some (TPrim "Null") -> TPrim "Undef"
      | _ ->
        raise (Typ_error (p, (PatTyp((fun p t -> sprintf "lookup hidden field with %s in:\n%s" (P.pretty p) 
          (string_of_typ t)), pat, orig_t))))
      end
    with Invalid_parent msg -> raise (Typ_error (p, FixedString msg))

  and subt env (cache : TPSet.t) s t : TPSet.t = 
    if TPSet.mem (s, t) cache then
      cache
    (* workaround for equal: functional value, due to lazy *)
    else if simpl_equiv s t then
      cache
    else
      let simpl_s = expose env (simpl_typ env s) in
      let simpl_t = expose env (simpl_typ env t) in
      if TPSet.mem (simpl_s, simpl_t) cache then cache else
      (* Printf.printf "Checking %s against %s\n" (string_of_typ simpl_s) (string_of_typ simpl_t); *)
      match simpl_s, simpl_t with
      | TUninit t', t2 -> begin match !t' with
        | None -> subt env cache (TPrim "Undef") t2
        | Some t1 -> subt env cache t1 t2
      end
      | t1, TUninit t' -> begin match !t' with
        | None -> subt env cache t1 (TPrim "Undef")
        | Some t2 -> subt env cache t1 t2
      end
      | _ -> let cache = TPSet.add (s, t) cache in match simpl_s, simpl_t with
        | TUninit _, _
        | _, TUninit _ -> failwith "Should not be possible!"
        | TRegex pat1, TRegex pat2 ->
          (* Printf.eprintf "Is %s <?: %s == " (P.pretty pat1) (P.pretty pat2); *)
          if P.is_subset (pat_env env) pat1 pat2 then begin
            (* Printf.eprintf "yes\n"; *)
            cache
          end else begin
              (* Printf.eprintf "no: \"%s\"\n" (match (P.example (P.subtract pat1 pat2)) with *)
              (* | None -> "CONTRADICTION!" *)
              (* | Some s -> s);  *)
              mismatched_typ_exn (TRegex pat1) (TRegex pat2)
            end
        | _, TIntersect (_, t1, t2) -> (* order matters -- right side must be split first! *)
            subt env (subt env cache s t1) s t2
        | TIntersect (_, s1, s2), _ -> 
          begin 
            try subt env cache s1 t
            with Not_subtype _ -> subt env cache s2 t
          end
        | TUnion (_, s1, s2), _ -> (* order matters -- left side must be split first! *)
          subt env (subt env cache s1 t) s2 t
        | _, TUnion (_, t1, t2) ->
          begin 
            try subt env cache s t1
            with Not_subtype _ -> subt env cache s t2
          end
        | _, TThis(TPrim "Unsafe") -> cache (* Can always mask the this parameter as Unsafe *)
        | TPrim "Null", TRef (_, TObject _)
        | TPrim "Null", TSource (_, TObject _)
        | TPrim "Null", TSink (_, TObject _) -> cache (* null should be a subtype of all object types *)
        | TThis (TId _ as id), t -> subt env cache (TThis (expose env id)) t
        | t, TThis(TRec _ as r) -> subt env cache t (TThis (expose env r))
        | TThis (TRef (_, s)), TThis (TRef (_, t))
        | TThis (TRef (_, s)), TThis (TSource (_, t))
        | TThis (TRef (_, s)), TThis (TSink (_, t))
        | TThis (TSource (_, s)), TThis (TRef (_, t))
        | TThis (TSource (_, s)), TThis (TSource (_, t))
        | TThis (TSource (_, s)), TThis (TSink (_, t))
        | TThis (TSink (_, s)), TThis (TRef (_, t))
        | TThis (TSink (_, s)), TThis (TSource (_, t))
        | TThis (TSink (_, s)), TThis (TSink (_, t))
        | TRef (_, s), TThis (TRef (_, t))
        | TRef (_, s), TThis (TSource (_, t))
        | TRef (_, s), TThis (TSink (_, t))
        | TSource (_, s), TThis (TRef (_, t))
        | TSource (_, s), TThis (TSource (_, t))
        | TSource (_, s), TThis (TSink (_, t))
        | TSink (_, s), TThis (TRef (_, t))
        | TSink (_, s), TThis (TSource (_, t))
        | TSink (_, s), TThis (TSink (_, t)) -> subt env cache s t (* ONLY HAVE TO GO ONE WAY ON THIS TYPES *)
        | TThis _, TThis _ -> mismatched_typ_exn s t
        | _, TThis t2 -> subt env cache s t2
        | TThis t1, _ -> subt env cache t1 t
        | TArrow (args1, v1, r1), TArrow (args2, v2, r2) ->
          begin
            match v1, v2 with
            | None, None ->
              (try List.fold_left2 (subt env) cache (r1 :: args2) (r2 :: args1)
               with Invalid_argument _ -> mismatched_typ_exn s t)
            | Some v1, Some v2 ->
              (try List.fold_left2 (subt env) cache (r1 :: args2 @ [v2]) (r2 :: args1 @ [v1])
               with Invalid_argument _ -> mismatched_typ_exn s t)
            | _ -> mismatched_typ_exn s t
          end
        | TId x, t -> 
          (try
             subt env cache (fst2 (IdMap.find x env)) t
           with Not_found -> Printf.printf "Cannot find %s in environment\n" x; raise Not_found)
        | t, TId x -> 
          (try
             subt env cache t (fst2 (IdMap.find x env))
           with Not_found -> Printf.printf "Cannot find %s in environment\n" x; raise Not_found)
        | TObject obj1, TObject obj2 ->
            subtype_object env cache obj1 obj2
        | TRef (_, s'), TRef (_, t') -> subt env (subt env cache s' t') t' s'
        | TSource (_, s), TSource (_, t) -> subt env cache s t
        | TSink (_, s), TSink (_, t) -> subt env cache t s
        | TRef (_, s), TSource (_, t) -> subt env cache s t
        | TRef (_, s), TSink (_, t) -> subt env cache t s
        | TForall (_, x1, s1, t1), TForall (_, x2, s2, t2) -> 
          (* Kernel rule *)
          (* TODO: ensure s1 = s2 *)
          let cache' = subt env (subt env cache s1 s2) s2 s1 in
          let t2 = typ_subst x2 (TId x1) t2 in
          let env' = IdMap.add x1 (s1, KStar) env in
          subt env' cache' t1 t2
        | _, TTop -> cache
        | TBot, _ -> cache
        | TLambda (_, [(x, KStar)], s), TLambda (_, [(y, KStar)], t) ->
          let env = IdMap.add x (TTop, KStar) env in
          let env = IdMap.add y (TTop, KStar) env in
          subt env cache s t
        | _ -> mismatched_typ_exn s t

  (* Check that an "extra" field is inherited *)
  and check_inherited env cache lang other_proto typ =
    subt env cache typ (inherits Pos.dummy env other_proto lang)

  and subtype_presence prop1 prop2 = match prop1, prop2 with
    | Present, Present 
    | Maybe, Maybe
    | Inherited, Inherited
    | Present , Maybe
    | Present, Inherited -> ()
    | _, _ -> raise (Not_subtype (FixedString"incompatible presence annotations"))

  and subtype_object env cache obj1 obj2 : TPSet.t =
    let lhs_absent = absent_pat obj1 in
    let rhs_absent = absent_pat obj2 in
    let check_simple_overlap ((pat1, pres1, t1), (pat2, pres2, t2)) cache = 
      if P.is_overlapped pat1 pat2 then
        begin
          subtype_presence pres1 pres2;
          (* Printf.printf "%s overlaps %s; checking subtypes of %s <: %s\n" *)
          (*   (P.pretty pat1) (P.pretty pat2) (string_of_typ t1) (string_of_typ t2); *)
          subt env cache t1 t2
        end
      else
        cache in
    let check_pat_containment () =
      (if not (P.is_subset (pat_env env) (possible_field_cover_pat obj2) 
                           (cover_pat obj1)) then
         match P.example (P.subtract (possible_field_cover_pat obj2)
                                     (cover_pat obj1)) with
         | Some ex -> raise (Not_subtype
                               (FixedString("fields on the RHS that are not on the LHS, e.g. " ^ ex ^ 
                                   "; cover_pat obj1 = " ^ (P.pretty (cover_pat obj1)) ^
                                   "; possible_pat obj1 = " ^ (P.pretty (possible_field_cover_pat obj1)))))
         | None -> failwith "error building counterexample for (2)");
      (if not (P.is_subset (pat_env env) rhs_absent lhs_absent); 
          then raise (Not_subtype (FixedString "subtype_object: violated 2-2"))) in
    let check_lhs_absent_overlap (rhs_pat, rhs_pres, rhs_prop) = 
      if P.is_overlapped rhs_pat lhs_absent then
        match rhs_pres with
          | Maybe | Inherited -> ()
          | _ -> raise (Not_subtype (FixedString "check_lhs_absent_overlap: LHS absent, RHS present")) in
    let check_rhs_inherited (rhs_pat, rhs_pres, rhs_typ) cache = 
      match rhs_pres with
      | Inherited -> 
          let lhs_typ = inherits Pos.dummy env (TObject obj1) rhs_pat in
          subt env cache lhs_typ rhs_typ
      | _ -> cache in
    (* try  *)
      let cache = 
        L.fold_right check_simple_overlap (L.pairs obj1.fields obj2.fields)
          cache in
      check_pat_containment ();
      L.iter check_lhs_absent_overlap obj2.fields;
      fold_right check_rhs_inherited obj2.fields cache
    (* with Not_subtype m -> *)
    (*   Printf.eprintf "Subtype failed for %s </: %s because\n%s\n" *)
    (*     (string_of_typ (TObject obj1)) (string_of_typ (TObject obj2)) (typ_error_details_to_string m); *)
    (*   raise (Not_subtype m) *)

  and subtypes env ss ts = 
    try 
      let _ = List.fold_left2 (subt env) TPSet.empty ss ts in
      true
    with 
      | Invalid_argument _ -> false (* unequal lengths *)
      | Not_subtype _ -> false

  and cache : TPSet.t ref = ref TPSet.empty 
  and subtype env s t = 
    try
      cache := subt env !cache s t;
      true
    with 
      | Not_subtype str -> false

  and typ_union cs s t = match subtype cs s t, subtype cs t s with
      true, true -> s (* t = s *)
    | true, false -> t (* s <: t *)
    | false, true -> s (* t <: s *)
    | false, false -> TUnion (None, s, t)

  and typ_intersect cs s t = match subtype cs s t, subtype cs t s with
    | true, true -> s
    | true, false -> s (* s <: t *)
    | false, true -> t
    | false, false -> TIntersect (None, s, t)

  let filter_typ (pred : typ -> bool) (typ : typ) = 
    let none_removed = ref true in
    let rec union (typs : typ list) n = match typs with
      | t1 :: ts -> fold_right (fun s t -> TUnion (n, s, t)) ts t1
      | _ -> failwith "expected non-empty list" in
    let combine n m = if n = None then m else n in
    let rec f t n_outer : typ list * string option = match t with
      | TUnion (n, s1, s2) -> 
        let n' = combine n n_outer in 
        let (fs1, n1) = f s1 n' in
        let (fs2, n2) = f s2 n' in
        (fs1 @ fs2, combine n' (combine n1 n2))
      | TIntersect (n, s1, s2) -> 
        let n' = combine n n_outer in
        begin match f s1 n', f s2 n' with
        | ([], n1), ([], n2) -> [], combine n' (combine n1 n2)
        | ([], n1), (typs, n2)
        | (typs, n1), ([], n2) -> typs, combine n' (combine n1 n2)
        | (typs1, n1), (typs2, n2) -> [TIntersect (n', union typs1 n1, union typs2 n2)], combine n' (combine n1 n2)
        end
      | _ -> if pred t then 
          [t], None
        else 
          begin
            none_removed := false;
            [], None
          end in
    let (typ_lst, _) = f typ None in
    (typ_lst, !none_removed)

  let is_object_typ t = match t with
    | TObject _ -> true
    | TUninit t -> begin match !t with
      | Some (TObject _) -> true
      | _ -> false
    end
    | _ -> false

  let object_typs (typ : typ) : typ list * bool = filter_typ is_object_typ typ
  
end

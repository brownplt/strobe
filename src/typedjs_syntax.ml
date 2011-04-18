open Prelude
open RegLang_syntax

exception Typ_error of pos * string

module RT = struct
  type t =
    | Num
    | Re of regex
    | Bool
    | Function
    | Object
    | Undefined

  let compare = Pervasives.compare

  open FormatExt

  let pp v = match v with
    | Num -> text "number"
    | Re re -> text ("string:" ^ (Pretty.string_of_re re))
    | Bool -> text "boolean"
    | Function -> text "function"
    | Object -> text "object"
    | Undefined -> text "undefined"

end

module RTSet = Set.Make (RT)
module RTSetExt = SetExt.Make (RTSet)

type constr = string

type prim =
  | Num
  | Int
  | Str
  | True
  | False
  | Undef
  | Null

type field = RegLang.regex * RegLang.fsm

type typ = 
  | TPrim of prim
  | TUnion of typ * typ
  | TIntersect of typ * typ
  | TArrow of typ list * typ
      (* The list holds everything that's typed.
         The second type is the prototype *)
  | TObject of (field * prop) list * typ
  (* | TSimpleObject of (field * prop) list *)
    (** If [obj] has type [TSimpleObject ((x, t)::rest)] then, [obj.x]
	gauranteed to produce a [t]-typed value. However, the type does not
	specify the field's position on the prototype chain. Therefore, setting
	[obj.x] might create a new field. *)
  | TRegex of field
  | TRef of typ
  | TSource of typ
  | TSink of typ
  | TTop
  | TBot
  | TForall of id * typ * typ (** [TForall (a, s, t)] forall a <: s . t *)
  | TId of id
  | TField
  | TRec of id * typ 
  | TSyn of id (** type synonym *)
  | TApp of typ * typ (** A forall to be substituted on normalization *)
and prop = 
  | PPresent of typ
  | PMaybe of typ
  | PAbsent
  | PErr

let typ_bool = TUnion (TPrim (True), TPrim (False))

let any_fld = (RegLang_syntax.any_str,
               RegLang.fsm_of_regex RegLang_syntax.any_str)

let mk_object_typ (fs : (field * prop) list) (star : prop option) proto : typ =
  let union (re1, _) re2 = if re2 = RegLang_syntax.Empty then re1 else 
    RegLang_syntax.Alt (re1, re2) in
  let union_re = List.fold_right union (map fst2 fs) RegLang_syntax.Empty in
  let rest_fsm = RegLang.negate (RegLang.fsm_of_regex union_re) in
    match star with
      | Some (PPresent t) ->
          TObject (((RegLang_syntax.Negate union_re, rest_fsm), 
                    PMaybe t)::fs, proto)
      | Some prop -> 
          TObject (((RegLang_syntax.Negate union_re, rest_fsm), 
                    prop)::fs, proto)
      | None ->
          TObject (((RegLang_syntax.Negate union_re, rest_fsm), 
                    PAbsent)::fs, proto)

let rec remove_this op = match op with
  | TArrow (a::aa, r) -> TArrow (aa, r)
  | TIntersect (t1, t2) -> TIntersect (remove_this t1, remove_this t2)
  | TForall (x, s, t) -> TForall (x, s, (remove_this t))
  | _ -> failwith "Removing this from something non-operatory"


type env_decl =
  | EnvClass of constr * constr option * typ
  | EnvBind of id * typ

type annotation =
    ATyp of typ
  | AConstructor of typ 
  | AUpcast of typ
  | ADowncast of typ
  | ATypAbs of id * typ
  | ATypApp of typ
  | AAssertTyp of typ
  | ACheat of typ

type ref_kind =
  | RefCell
  | SourceCell
  | SinkCell

(** A type written by a programmer. This type may be incorrect and needs
    to be checked. *)
type writ_typ = WrittenTyp of typ

type func_info = {
  func_typ : writ_typ;
  func_owned: IdSet.t;
  func_loop : bool;
}


(** Typed JavaScript expressions. Additional well-formedness criteria are
    inline. *)
type exp
  = EConst of pos * JavaScript_syntax.const
  | EBot of pos
  | EAssertTyp of pos * writ_typ * exp
  | EArray of pos * exp list
  | EEmptyArray of pos * writ_typ
  | EObject of pos * (string * exp) list
  | EId of pos * id
  | EBracket of pos * exp * exp
  | EUpdate of pos * exp * exp * exp
  | ENew of pos * id * exp list
  | EPrefixOp of pos * id * exp
  | EInfixOp of pos * id * exp * exp
  | EIf of pos * exp * exp * exp
  | EApp of pos * exp * exp list
  | EFunc of pos * id list * func_info * exp
  | ELet of pos * id * exp * exp
  | ERec of (id * writ_typ * exp) list * exp
  | ESeq of pos * exp * exp
  | ELabel of pos * id * writ_typ * exp 
  | EBreak of pos * id * exp
  | ETryCatch of pos * exp * id * exp
  | ETryFinally of pos * exp * exp
  | EThrow of pos * exp
  | ETypecast of pos * RTSet.t * exp
  | ERef of pos * ref_kind * exp
  | EDeref of pos * exp
  | ESetRef of pos * exp * exp
  | ESubsumption of pos * writ_typ * exp
  | EDowncast of pos * writ_typ * exp
  | ETypAbs of pos * id * writ_typ * exp 
  | ETypApp of pos * exp * typ
  | EForInIdx of pos
  | ECheat of pos * writ_typ * exp

(******************************************************************************)

module Typ = struct

  let rec match_func_typ (typ : typ) : (typ list * typ) option = match typ with
    | TForall (_, _, t) -> match_func_typ t
    | TArrow (args, ret) -> Some (args, ret)
    | _ -> None

end

module Exp = struct

  type t = exp

  let pos exp = match exp with
    | EConst (p, _) -> p
    | EBot p -> p
    | EAssertTyp (p, _, _) -> p
    | EArray (p, _) -> p
    | EObject (p, _) -> p
    | EId (p, _) -> p
    | EBracket (p, _, _) -> p
    | EUpdate (p, _, _, _) -> p
    | ENew (p, _, _) -> p
    | EPrefixOp (p, _, _) -> p
    | EInfixOp (p, _, _, _) -> p
    | EIf (p, _, _, _) -> p
    | EApp (p, _, _) -> p
    | EFunc (p, _, _, _) -> p
    | ELet (p, _, _, _) -> p
    | ERec (_, _) -> failwith "Exp.pos of ERec"
    | ESeq (p, _, _) -> p
    | ELabel (p, _, _, _) -> p
    | EBreak (p, _, _) -> p
    | ETryCatch (p, _, _, _) -> p
    | ETryFinally (p, _, _) -> p
    | EThrow (p, _) -> p
    | ETypecast (p, _, _) -> p
    | ERef (p, _, _) -> p
    | EDeref (p, _) -> p
    | ESetRef (p, _, _) -> p
    | ESubsumption (p, _, _) -> p
    | EDowncast (p, _, _) -> p
    | EEmptyArray (p, _) -> p
    | ETypApp (p, _, _) -> p
    | ETypAbs (p, _, _, _) -> p
    | EForInIdx p -> p
    | ECheat (p, _, _) -> p
end

module Pretty = struct

  open Format
  open FormatExt

  let fld field = match field with
    | (re, fsm) -> squish [text "/"; RegLang_syntax.Pretty.p_re re; text "/"]

  let rec typ t  = match t with
    | TTop -> text "Any"
    | TBot -> text "DoesNotReturn"
    | TPrim p -> text begin match p with
       | Num -> "Num"
       | Int -> "Int"
       | True -> "True"
       | False -> "False"
       | Str -> "Str"
       | Null -> "Null"
       | Undef -> "Undef"
      end
    | TApp (t1, t2) -> horz [typ t1; text "<"; typ t2; text ">"]
    | TRegex (regex, fsm) -> 
        squish [text "/"; RegLang_syntax.Pretty.p_re regex; text "/"]
    | TUnion (t1, t2) -> horz [typ t1; text "+"; typ t2]
    | TIntersect (t1, t2) -> horz [typ t1; text "&"; typ t2]
    | TArrow (tt::arg_typs, r_typ) ->
        horz[ brackets (typ tt);
              horz (intersperse (text "*") 
                      (map (fun at -> begin match at with
                              | TArrow _ -> parens (typ at)
                              | _ -> typ at 
                            end) arg_typs));
              text "->";
              typ r_typ ]
    | TArrow (arg_typs, r_typ) ->
        horz[ horz (intersperse (text "*") 
                      (map (fun at -> begin match at with
                              | TArrow _ -> parens (typ at)
                              | _ -> typ at 
                            end) arg_typs));
              text "->";
              typ r_typ ]
    | TObject (fs, proto) ->
        let f (k, p) = horz [ fld k; text ":"; prop p ] in
          braces (horz ([text "proto:"; typ proto; text ";"]@
                          (intersperse (text ",") (map f fs))))
    | TRef s -> horz [ text "ref"; parens (typ s) ]
    | TSource s -> horz [ text "source"; parens (typ s) ]
    | TSink s -> horz [ text "sink"; parens (typ s) ]
    | TForall (x, s, t) -> 
        horz [ text "forall"; text x; text "<:"; typ s; text "."; typ t ]
    | TId x -> text x
    | TSyn (name) -> text name
    | TField -> text "field"
    | TRec (x, t) -> horz [ text x; text "."; typ t ]
  and prop p = match p with
    | PPresent t -> typ t
    | PMaybe t -> horz [ text "maybe"; typ t ]
    | PAbsent -> text "_"
    | PErr -> text "BAD"

  let writ_typ wt = match wt with
    | WrittenTyp t -> typ t

  let rec exp e = match e with
    | EConst (_, c) -> JavaScript.Pretty.p_const c
    | EBot _ -> text "bot"
    | EAssertTyp (_, t, e) ->
        parens (vert [ text "assert-typ"; parens (writ_typ t); exp e ])
    | EEmptyArray _ -> text "[ ]"
    | EArray (_, es) -> brackets (horz (map exp es))
    | EObject (_, ps) -> brackets (vert (map fld ps))
    | EId (_, x) -> text x
    | EBracket (_, e1, e2) -> squish [ exp e1; brackets (exp e2) ]
    | EUpdate (_, e1, e2, e3) -> 
        squish [ exp e1; 
                 brackets (squish [ exp e2; text ":="; exp e3])]
    | ENew (_, c_id, args) ->
        parens (horz (text "new" :: text c_id :: map exp args))
    | EIf (_, e1, e2, e3) ->
        parens (vert [ horz [ text "if"; exp e1 ]; exp e2; exp e3 ])
    | EApp (_, f, args) -> parens (horz (exp f :: map exp args))
    | EFunc (_, args, t, body) ->
      parens (vert [ horz [ text "fun"; parens (horz (map text args)); 
                            text ":"; writ_typ t.func_typ;
                            IdSetExt.p_set text t.func_owned;
                          ];
                       exp body])
    | ELet (_, x, bound, body) ->
        parens (vert [ horz [ text "let";
                              parens (vert (map bind [(x, bound)]))];
                       exp body ])
    | ERec (binds, body) ->
        parens (vert [ horz [ text "rec"; parens (vert (map rec_bind binds)) ];
                       exp body ])
    | ESeq (_, e1, e2) -> parens (vert [ sep [ text "seq"; exp e1 ]; exp e2 ])
    | ELabel (_, x, t, e) -> parens (vert [ text "label"; text x; exp e ])
    | EBreak (_, x, e) -> parens (vert [ text "break"; text x; exp e ])
    | ETryCatch (_, body, x, catch) ->
        parens (vert [ text "try"; exp body; 
                       parens (vert [ text "catch"; text x; exp body ])])
    | ETryFinally (_, body, finally) ->
        parens (vert [ text "try"; exp body;
                       parens (vert [ text "finally"; exp finally ]) ])
    | EThrow (_, e) -> parens (vert [ text "throw"; exp e ])
    | EPrefixOp (_, op, e) -> parens (vert [ text op; exp e ])
    | EInfixOp (_, op, e1, e2) -> parens (horz [ text op; exp e1; exp e2 ])
    | ETypecast (_, t, e) ->
        parens (vert [ text "cast"; RTSetExt.p_set RT.pp t; exp e ])
    | ERef (_, _, e) -> parens (horz [ text "ref"; exp e ])
    | EDeref (_, e) -> parens (horz [ text "deref"; exp e ])
    | ESetRef (_, e1, e2) -> parens (horz [ text "set-ref!"; exp e1; exp e2 ])
    | ESubsumption (_, t, e) ->
        parens (vert [ text "upcast"; parens (writ_typ t); exp e ])
    | EDowncast (_, t, e) ->
        parens (vert [ text "downcast"; parens (writ_typ t); exp e ])
    | ETypApp (_, e, t) -> parens (horz [ text "typ-app"; exp e; typ t ])
    | ETypAbs (_, x, t, e) -> 
        parens (horz [ text "typ-abs"; text x; text "<:"; writ_typ t; exp e ])
    | EForInIdx _ -> text "for-in-idx"
    | ECheat (_, t, e) -> parens (horz [ text "cheat"; writ_typ t; exp e ])

  and fld (s, e) =
    parens (horz [ text s; text ":"; exp e ])

  and bind (x, e) = 
    parens (horz [text x; exp e])

  and rec_bind (x, t, e) = 
    parens (horz [text x; text ":"; writ_typ t; exp e])

  let p_typ = typ

  let p_exp = exp

  let p_prop = prop

  let pp_typ ppf t = p_typ t ppf
   
end

let string_of_typ = FormatExt.to_string Pretty.p_typ
let string_of_exp = FormatExt.to_string Pretty.p_exp
let string_of_prop = FormatExt.to_string Pretty.p_prop
let assigned_free_vars (e : exp) = 
  let rec exp : exp -> IdSet.t = function
    | EConst _ -> IdSet.empty
    | EBot _ -> IdSet.empty
    | EAssertTyp (_, _, e) -> exp e
    | EArray (_, es) -> IdSetExt.unions (map exp es)
    | EEmptyArray _ -> IdSet.empty
    | EObject (_, fs) -> IdSetExt.unions (map (fun (_, e) -> exp e) fs)
    | EId _ -> IdSet.empty
    | EBracket (_, e1, e2) -> IdSet.union (exp e1) (exp e2)
    | EUpdate (_, e1, e2, e3) -> IdSetExt.unions [ exp e1; exp e2; exp e3 ]
    | ENew (_, _, es) -> IdSetExt.unions (map exp es)
    | EPrefixOp (_, _, e) -> exp e
    | EInfixOp (_, _, e1, e2) -> IdSet.union (exp e1) (exp e2)
    | EIf (_, e1, e2, e3) -> IdSetExt.unions [ exp e1; exp e2; exp e3 ]
    | EApp (_, e, es) -> IdSetExt.unions (map exp (e :: es))
    | EFunc (_, args, _, e) -> fold_right IdSet.remove args (exp e)
    | ELet (_, x, e1, e2) ->  IdSet.union (exp e1) (IdSet.remove x (exp e2))
    | ERec (binds, e) ->
      let (xs, es) = 
        fold_right (fun (x, _, e) (xs, es) -> (x::xs, e::es))
          binds ([], []) in
      fold_right IdSet.remove xs (IdSetExt.unions (map exp (e :: es)))
    | ESeq (_, e1, e2) -> IdSet.union (exp e1) (exp e2)
    | ELabel (_, _, _, e) -> exp e
    | EBreak (_, _, e) -> exp e
    | ETryCatch (_, e1, x, e2) -> IdSet.union (exp e1) (IdSet.remove x (exp e2))
    | ETryFinally (_, e1, e2) -> IdSet.union (exp e1) (exp e2)
    | EThrow (_, e) -> exp e
    | ETypecast (_, _, e) -> exp e
    | ERef (_, _, e) -> exp e
    | EDeref (_, e) -> exp e
    | ESetRef (_, EId (_, x), e) -> IdSet.add x (exp e)
    | ESetRef (_, e1, e2) -> IdSet.union (exp e1) (exp e2)
    | ESubsumption (_, _, e) -> exp e
    | EDowncast (_, _, e) -> exp e
    | ETypAbs (_, _, _, e) -> exp e
    | ETypApp (_, e, _) -> exp e
    | EForInIdx _ -> IdSet.empty
    | ECheat _ -> IdSet.empty
  in exp e

let unique_ids (prog : exp) : exp * (string, string) Hashtbl.t = 
  let module H = Hashtbl in
  let ht : (string, string) H.t = H.create 200 in
  let name : id -> id =
    let next = ref 0 in
    fun old_name ->
      let new_name = "#" ^ (string_of_int !next) in
      H.add ht new_name old_name;
      incr next; 
      new_name in
  let find x env = 
    if IdMap.mem x env then IdMap.find x env
    else x (* assume global *) in
  let rec exp (env : id IdMap.t) = function
    | EConst (p, c) -> EConst (p, c)
    | EBot p -> EBot p
    | EAssertTyp (p, t, e) -> EAssertTyp (p, t, exp env e)
    | EArray (p, es) -> EArray (p, map (exp env) es)
    | EEmptyArray (p, t) -> EEmptyArray (p, t)
    | EObject (p, flds) -> EObject (p, map (second2 (exp env)) flds)
    | EId (p, x) -> EId (p, find x env)
    | EBracket (p, e1, e2) -> EBracket (p, exp env e1, exp env e2)
    | EUpdate (p, e1, e2, e3) -> EUpdate (p, exp env e1, exp env e2, exp env e3)
    | ENew (p, x, es) -> ENew (p, find x env, map (exp env) es)
    | EPrefixOp (p, op, e) -> EPrefixOp (p, op, exp env e)
    | EInfixOp (p, op, e1, e2) -> EInfixOp (p, op, exp env e1, exp env e2)
    | EIf (p, e1, e2, e3) -> EIf (p, exp env e1, exp env e2, exp env e3)
    | EApp (p, e, es) -> EApp (p, exp env e, map (exp env) es)
    | EFunc (p, xs, fi, e) ->
      let xs' = map name xs in
      EFunc (p, xs', fi, exp (List.fold_right2 IdMap.add xs xs' env) e)
    | ELet (p, x, e1, e2) ->
      let x' = name x in
      ELet (p, x', exp env e1, exp (IdMap.add x x' env) e2)
    | ERec (binds, body) ->
      let (xs, xs') =
        fold_right 
          (fun (x, _, _) (xs, xs') -> (x::xs, (name x)::xs'))
          binds ([], []) in
      let env = List.fold_right2 IdMap.add xs xs' env in
      ERec (map (fun (x, t, e) -> (find x env, t, exp env e)) binds,
            exp env body)
    | ESeq (p, e1, e2) -> ESeq (p, exp env e1, exp env e2)
    | ELabel (p, x, t, e) -> ELabel (p, x, t, exp env e)
    | EBreak (p, x, e) -> EBreak (p, x, exp env e)
    | ETryCatch (p, e1, x, e2) ->
      let x' = name x in
      ETryCatch (p, exp env e1, x', exp env e2)
    | ETryFinally (p, e1, e2) -> ETryFinally (p, exp env e1, exp env e2)
    | EThrow (p, e) -> EThrow (p, exp env e)
    | ETypecast (p, s, e) -> ETypecast (p, s, exp env e)
    | ERef (p, k, e) -> ERef (p, k, exp env e)
    | EDeref (p, e) -> EDeref (p, exp env e)
    | ESetRef (p, e1, e2) -> ESetRef (p, exp env e1, exp env e2)
    | ESubsumption (p, t, e) -> ESubsumption (p, t, exp env e)
    | EDowncast (p, t, e) -> EDowncast (p, t, exp env e)
    | ETypAbs (p, x, t, e) -> ETypAbs (p, x, t, exp env e)
    | ETypApp (p, e, t) -> ETypApp (p, exp env e, t)
    | EForInIdx p -> EForInIdx p
    | ECheat (p, t, e) -> ECheat (p, t, e)
  in (exp IdMap.empty prog, ht)

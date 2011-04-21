open Prelude

exception Typ_error of pos * string

module P = Sb_strPat

module RT = struct
  type t =
    | Num
    | Re of Sb_strPat.t
    | Bool
    | Function
    | Object
    | Undefined

  let compare = Pervasives.compare

  open FormatExt

  let pp v = match v with
    | Num -> text "number"
    | Re pat -> text ("string:" ^ (Sb_strPat.pretty pat))
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
  | True
  | False
  | Undef
  | Null

type field = Sb_strPat.t
type pat = Sb_strPat.t

let proto_str = "proto"

let proto_pat = Sb_strPat.singleton proto_str

type kind = 
  | KStar
  | KArrow of kind * kind

type typ = 
  | TPrim of prim
  | TUnion of typ * typ
  | TIntersect of typ * typ
  | TArrow of typ list * typ
      (* The list holds everything that's typed.
         The second type is the prototype *)
  | TObject of (field * prop) list
  | TRegex of field
  | TRef of typ
  | TSource of typ
  | TSink of typ
  | TTop
  | TBot
  | TForall of id * typ * typ (** [TForall (a, s, t)] forall a <: s . t *)
  | TId of id
  | TRec of id * typ 
  | TLambda of id * kind * typ
  | TApp of typ * typ (** type operator application *)

and prop = 
  | PPresent of typ
  | PMaybe of typ
  | PAbsent

let typ_bool = TUnion (TPrim (True), TPrim (False))

let any_fld = Sb_strPat.all


type ref_kind =
  | RefCell
  | SourceCell
  | SinkCell


type func_info = {
  func_typ : typ;
  func_owned: IdSet.t;
  func_loop : bool;
}


(** Typed JavaScript expressions. Additional well-formedness criteria are
    inline. *)
type exp
  = EConst of pos * JavaScript_syntax.const
  | EBot of pos
  | EAssertTyp of pos * typ * exp
  | EArray of pos * exp list
  | EEmptyArray of pos * typ
  | EObject of pos * (string * exp) list
  | EId of pos * id
  | EBracket of pos * exp * exp
  | EUpdate of pos * exp * exp * exp
  | EPrefixOp of pos * id * exp
  | EInfixOp of pos * id * exp * exp
  | EIf of pos * exp * exp * exp
  | EApp of pos * exp * exp list
  | EFunc of pos * id list * func_info * exp
  | ELet of pos * id * exp * exp
  | ERec of (id * typ * exp) list * exp
  | ESeq of pos * exp * exp
  | ELabel of pos * id * typ * exp 
  | EBreak of pos * id * exp
  | ETryCatch of pos * exp * id * exp
  | ETryFinally of pos * exp * exp
  | EThrow of pos * exp
  | ETypecast of pos * RTSet.t * exp
  | ERef of pos * ref_kind * exp
  | EDeref of pos * exp
  | ESetRef of pos * exp * exp
  | ESubsumption of pos * typ * exp
  | EDowncast of pos * typ * exp
  | ETypAbs of pos * id * typ * exp 
  | ETypApp of pos * exp * typ
  | ECheat of pos * typ * exp

(******************************************************************************)

(** Types written by users. *)
module WritTyp = struct

  type t = 
    | Str
    | Bool
    | Prim of prim
    | Union of t * t
    | Inter of t * t
    | Arrow of t option * t list * t (** [Arrow (this, args, result)] *)
    | Object of f list
    | SimpleObject of f list
    | Pat of field
    | Ref of t
    | Source of t
    | Top
    | Bot
    | Id of id
    | Forall of id * t * t
    | Rec of id * t
    | Syn of id
    | Lambda of id * t
    | App of t * t
       
  and f = 
    | Present of pat * t
    | Maybe of pat * t
    | Absent of pat
    | Skull of pat
    | Star of t option

  let rec remove_this op = match op with
    | Arrow (_, aa, r) -> Arrow (None, aa, r)
    | Inter (t1, t2) -> Inter (remove_this t1, remove_this t2)
    | Forall (x, s, t) -> Forall (x, s, remove_this t)
    | _ -> failwith "remove_this : illegal argument"

end

type env_decl =
  | EnvBind of pos * id * WritTyp.t
  | EnvType of pos * id * WritTyp.t

type annotation =
  | ATyp of WritTyp.t
  | AUpcast of WritTyp.t
  | ADowncast of WritTyp.t
  | ATypAbs of id * WritTyp.t
  | ATypApp of WritTyp.t
  | AAssertTyp of WritTyp.t
  | ACheat of WritTyp.t



module Typ = struct

  let rec forall_arrow (typ : typ) : (id list * typ) option = match typ with
    | TArrow _ -> Some ([], typ)
    | TForall (x, _, typ') -> begin match forall_arrow typ' with
	| None -> None
	| Some (xs, t) -> Some (x :: xs, t)
    end
    | _ -> None

  let rec match_func_typ (typ : typ) : (typ list * typ) option = match typ with
    | TForall (_, _, t) -> match_func_typ t
    | TArrow (args, ret) -> Some (args, ret)
    | _ -> None

  let is_present (fld : (pat * prop)) = match fld with
    | (_, PPresent _) -> true
    | _ -> false



  (** [obj_cover t] returns a pattern that matches the strings in the object's
      domain. When [t] is a [TObject _], it excludes the prototype. *)
  let obj_cover (typ : typ) = match typ with
    | TObject flds ->
      fold_left P.union P.empty (map fst2 flds)
    | _ ->
      raise (Invalid_argument "obj_cover requires an object type")

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
    | ECheat (p, _, _) -> p
end

module Pretty = struct

  open Format
  open FormatExt

  let rec typ t  = match t with
    | TTop -> text "Any"
    | TBot -> text "DoesNotReturn"
    | TPrim p -> text begin match p with
       | Num -> "Num"
       | Int -> "Int"
       | True -> "True"
       | False -> "False"
       | Null -> "Null"
       | Undef -> "Undef"
    end
    | TLambda (x, k, t) -> 
      horz [ text "Lambda "; text x; typ t ]
    | TApp (t1, t2) -> horz [typ t1; text "<"; typ t2; text ">"]
    | TRegex pat -> 
        squish [text "/"; text (Sb_strPat.pretty pat); text "/"]
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
    | TObject flds -> braces (vert (map field flds))
    | TRef s -> horz [ text "Ref"; parens (typ s) ]
    | TSource s -> horz [ text "Src"; parens (typ s) ]
    | TSink s -> horz [ text "Snk"; parens (typ s) ]
    | TForall (x, s, t) -> 
        horz [ text "forall"; text x; text "<:"; typ s; text "."; typ t ]
    | TId x -> text x
    | TRec (x, t) -> horz [ text "rec"; text x; text "."; typ t ]

  and field  (k, p) = horz [ text (Sb_strPat.pretty k); text ":"; prop p;
			     text "," ]

  and prop p = match p with
    | PPresent t -> typ t
    | PMaybe t -> horz [ text "maybe"; typ t ]
    | PAbsent -> text "_"

  let rec exp e = match e with
    | EConst (_, c) -> JavaScript.Pretty.p_const c
    | EBot _ -> text "bot"
    | EAssertTyp (_, t, e) ->
        parens (vert [ text "assert-typ"; parens (typ t); exp e ])
    | EEmptyArray _ -> text "[ ]"
    | EArray (_, es) -> brackets (horz (map exp es))
    | EObject (_, ps) -> brackets (vert (map fld ps))
    | EId (_, x) -> text x
    | EBracket (_, e1, e2) -> squish [ exp e1; brackets (exp e2) ]
    | EUpdate (_, e1, e2, e3) -> 
        squish [ exp e1; 
                 brackets (squish [ exp e2; text ":="; exp e3])]
    | EIf (_, e1, e2, e3) ->
        parens (vert [ horz [ text "if"; exp e1 ]; exp e2; exp e3 ])
    | EApp (_, f, args) -> parens (horz (text "app" :: exp f :: map exp args))
    | EFunc (_, args, t, body) ->
      parens (vert [ horz [ text "fun"; parens (horz (map text args)); 
                            text ":"; typ t.func_typ;
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
        parens (vert [ text "upcast"; parens (typ t); exp e ])
    | EDowncast (_, t, e) ->
        parens (vert [ text "downcast"; parens (typ t); exp e ])
    | ETypApp (_, e, t) -> parens (horz [ text "typ-app"; exp e; typ t ])
    | ETypAbs (_, x, t, e) -> 
        parens (horz [ text "typ-abs"; text x; text "<:"; typ t; exp e ])
    | ECheat (_, t, e) -> parens (horz [ text "cheat"; typ t; exp e ])

  and fld (s, e) =
    parens (horz [ text s; text ":"; exp e ])

  and bind (x, e) = 
    parens (horz [text x; exp e])

  and rec_bind (x, t, e) = 
    parens (horz [text x; text ":"; typ t; exp e])

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
    | ECheat (p, t, e) -> ECheat (p, t, e)
  in (exp IdMap.empty prog, ht)

open Prelude
open Typedjs_syntax
open Typedjs_types 
open Typedjs_pretty
open Typedjs_stxutil

module JS = JavaScript_syntax (* needed for operators *)

module type EnvType = sig
  
  type env

  val empty_env : env

  val bind_id : id -> typ -> env -> env

  val bind_lbl : id -> typ -> env -> env

  val lookup_id : id -> env -> typ

  val lookup_lbl : id -> env -> typ

  val assignable_ids : env -> IdSet.t

  val new_assignable_id : id -> env -> env

  val remove_assigned_ids : IdSet.t -> env -> env

end

module Env : EnvType = struct

  type env = { id_typs : typ IdMap.t; 
               lbl_typs : typ IdMap.t;
               asgn_ids : IdSet.t }


  let empty_env = { id_typs = IdMap.empty;
                    lbl_typs = IdMap.empty;
                    asgn_ids = IdSet.empty }

  let bind_id x t env  = { env with id_typs = IdMap.add x t env.id_typs }

  let bind_lbl x t env = { env with lbl_typs = IdMap.add x t env.lbl_typs }

  let lookup_id x env = IdMap.find x env.id_typs

  let lookup_lbl x env = IdMap.find x env.lbl_typs

  let assignable_ids env = env.asgn_ids

  let new_assignable_id x env = { env with asgn_ids = IdSet.add x env.asgn_ids }

  let remove_assigned_ids assigned_ids env =
    { env with asgn_ids = IdSet.diff env.asgn_ids assigned_ids }

end
 

let typ_error (p : pos) (s : string) : 'a =
  failwith ("type error at " ^ string_of_position p ^ ": " ^ s)

let tc_arith (p : pos) (t1 : typ) (t2 : typ) (int_args : bool) 
    (num_result : bool) : typ =
  let result_typ = 
    if num_result = true then typ_num
    else if subtype t1 typ_int then t2
    else t1 in
    if (int_args && subtype t1 typ_int && subtype t2 typ_int) ||
      (not int_args && subtype t1 typ_num && subtype t2 typ_num) then
      result_typ
    else typ_error p
      (match int_args with
           true -> 
             sprintf "operator expects arguments of type Int, but arguments \
               have type %s and %s" (string_of_typ t1) (string_of_typ t2)
         | false -> 
             sprintf "operator expects Int or Double arguments, but arguments \
               have type %s and %s" (string_of_typ t1) (string_of_typ t2))

let tc_cmp (p : pos) (lhs : typ) (rhs : typ) : typ = 
  if (subtype lhs typ_str && subtype rhs typ_str) || 
    (subtype lhs typ_num && subtype rhs typ_num) then
      typ_bool
  else
    typ_error p "comparison type error"

let rec tc_exp (env : Env.env) exp = match exp with
    EString _ -> typ_str
  | ERegexp _ -> typ_regexp
  | ENum _ -> typ_num
  | EInt _ -> typ_int
  | EBool _ -> typ_bool
  | ENull _ -> typ_null
  | EUndefined _ -> typ_undef
  | EId (p, x) ->
      (try Env.lookup_id x env
       with Not_found -> typ_error p ("identifier " ^ x ^ " is unbound"))
  | ELet (_, x, e1, e2) ->
      let t = tc_exp env e1
      and x_available = not (IdSet.mem x (local_av_exp e2)) in
      let env = Env.bind_id x t env in
      let env = Env.remove_assigned_ids (av_exp e1) env in
      let env = if x_available then Env.new_assignable_id x env else env in
        tc_exp env e2
  | ESeq (_, e1, e2) ->
      let _ = tc_exp env e1 in
        tc_exp env e2
  | ELabel (p, l, t, e) -> 
      let s = tc_exp (Env.bind_lbl l t env) e in
        if subtype s t then t
        else typ_error p "label type mismatch"
  | EBreak (p, l, e) ->
      let s = (try Env.lookup_lbl l env
               with Not_found -> typ_error p ("label " ^ l ^ " is not defined"))
      and t = tc_exp env e in
        if subtype t s then TBot
        else typ_error p 
          (match l with
               "%return" -> sprintf 
                 "this expression has type %s, but the function\'s return type \
                  is %s" (string_of_typ t) (string_of_typ s)
             | _ -> (* This should not happen. Breaks to labels always have 
                       type typ_undef *)
                 sprintf "this expression has type %s, but the label %s has \
                          type %s" (string_of_typ t) l (string_of_typ s))
  | ETryCatch (_, e1, x, e2) ->
      let t1 = tc_exp env e1
      and t2 = tc_exp (Env.bind_id x TTop env) e2 in
        typ_union t1 t2
  | ETryFinally (_, e1, e2) -> 
      let _ = tc_exp env e1 in
        tc_exp env e2
  | EThrow (_, e) -> 
      let _ = tc_exp env e in
        TBot
  | ETypecast (_, t, e) -> 
      let _ = tc_exp env e in
        t
  | EArray (p, []) -> 
      typ_error p "an empty array literal requires a type annotation"
  | EArray (_, e :: es) -> 
      let u = fold_left typ_union (tc_exp env e) (tc_exps env es) in
        TApp ("Array", [u])
  | EIf (p, e1, e2, e3) -> begin
      match tc_exp env e1, tc_exp env e2, tc_exp env e3 with
          TApp ("Boolean", []), t2, t3 -> typ_union t2 t3
        | t, _, _ -> typ_error p "test expression must have type boolean"
    end
(*  | EObject of 'a * (string * 'a exp) list
  | EThis of 'a *)
(*
  | EBracket of 'a * 'a exp * 'a exp
  | ENew of 'a * 'a exp * 'a exp list *)
  | EPrefixOp (p, op, e) -> begin match op, tc_exp env e with
        JS.PrefixLNot, TApp ("Boolean", []) -> typ_bool
      | JS.PrefixBNot, TApp ("Int", []) -> typ_int
      | JS.PrefixBNot, TApp ("Number", []) ->
          (** JavaScript converts floats to integers before doing a bitwise
              negation. E.g. [~(1.0)], [~1], and [-2] are equivalent. *)
          typ_int 
      | JS.PrefixPlus, TApp ("Int", []) -> typ_int
      | JS.PrefixPlus, TApp ("Number", []) -> typ_num 
      | JS.PrefixMinus, TApp ("Int", []) -> typ_int
      | JS.PrefixMinus, TApp ("Number", []) -> typ_num 
      | JS.PrefixTypeof, _ -> typ_str
      | JS.PrefixVoid, _ -> typ_undef
          (* | PrefixDelete *)
    end
   | EInfixOp (p, op, e1, e2) -> 
       let t1 = tc_exp env e1
       and t2 = tc_exp env e2 in
       begin match op with
           JS.OpLT -> tc_cmp p t1 t2
         | JS.OpLEq -> tc_cmp p t1 t2
         | JS.OpGT -> tc_cmp p t1 t2
         | JS.OpGEq -> tc_cmp p t1 t2
         | JS.OpIn -> failwith "OpIn NYI"
         | JS.OpInstanceof -> failwith "instanceof NYI"
         | JS.OpEq -> typ_bool
         | JS.OpNEq -> typ_bool
         | JS.OpStrictEq -> typ_bool
         | JS.OpStrictNEq -> typ_bool
         | JS.OpLAnd -> typ_union t2 typ_bool
         | JS.OpLOr -> typ_union t1 t2
         | JS.OpMul -> tc_arith p t1 t2 false false
         | JS.OpDiv -> tc_arith p t1 t2 false true
         | JS.OpMod -> tc_arith p t1 t2 false true
         | JS.OpSub -> tc_arith p t1 t2 false false
         | JS.OpLShift -> tc_arith p t1 t2 true false
         | JS.OpSpRShift -> tc_arith p t1 t2 true false
         | JS.OpZfRShift -> tc_arith p t1 t2 true false
         | JS.OpBAnd -> tc_arith p t1 t2 true false
         | JS.OpBXor -> tc_arith p t1 t2 true false
         | JS.OpBOr -> tc_arith p t1 t2 true false
         | JS.OpAdd -> begin match subtype t1 typ_str, subtype t2 typ_str with
               true, _ -> typ_str
             | _, true -> typ_str
             | _ -> tc_arith p t1 t2 false false
           end
       end
   | EAssign (p, LVar (_, x), e) -> 
       let s = (try Env.lookup_id x env 
                with Not_found -> typ_error p (sprintf "%s is unbound" x))
       and t = tc_exp env e in
         if subtype t s && subtype s t then t
         else typ_error p 
           (sprintf "%s has type %s, but the right-hand side of this \
              assignment has type %s" x (string_of_typ s) (string_of_typ t))
(*   | EAssign of 'a * 'a lvalue * 'a exp  *)
  | EApp (p, f, args) -> begin match tc_exp env f with
        TArrow (_, expected_typs, result_typ) ->
          let arg_typs = tc_exps env args in
            if subtypes arg_typs expected_typs then result_typ 
            else if List.length args = List.length expected_typs then
              let typ_pairs = List.combine arg_typs expected_typs in
              let arg_ix = ref 1 in
              let find_typ_err (arg_typ, expected_typ) = 
                if subtype arg_typ expected_typ then (incr arg_ix; false)
                else true in
              let arg_typ, expected_typ = List.find find_typ_err typ_pairs in
                typ_error p
                  (sprintf "argument %d has type %s, but the function expects \
                     an argument of type %s" !arg_ix (string_of_typ arg_typ)
                     (string_of_typ expected_typ))
            else typ_error p
              (sprintf "arity-mismatch: the function expects %d arguments, but \
                      this application supplies %d arguments"
                 (List.length expected_typs) (List.length args))
      | _ -> typ_error p "expected a function"
    end
  | ERec (binds, body) -> 
      let f env (x, t, _) = Env.bind_id x t env in
      let env = ref (fold_left f env binds) in
      let tc_bind (x, t, e) =
        let s = tc_exp !env e in
          env := Env.remove_assigned_ids (av_exp e) !env;
          if subtype s t then ()
          else (* this should not happen; rec-annotation is a copy of the
                  function's type annotation. *)
            failwith (sprintf "%s is declared to have type %s, but the bound \
                             expression has type %s" x (string_of_typ t)
                        (string_of_typ s)) in
        List.iter tc_bind binds;
        tc_exp !env body
  | EFunc (p, args, fn_typ, body) -> 
      eprintf "Assignable ids are:\n";
      IdSetExt.pretty Format.err_formatter (Format.pp_print_string) 
        (Env.assignable_ids env);
      begin match fn_typ with
        TArrow (_, arg_typs, result_typ) ->
          if List.length arg_typs = List.length args then ()
          else typ_error p "not all arguments have types";
          let bind_arg env (x, t) = Env.bind_id x t env in
          let env' = fold_left bind_arg env (List.combine args arg_typs) in
          let body_typ = tc_exp env' body in
            if subtype body_typ result_typ then fn_typ
            else typ_error p
              (sprintf "function body has type %s, but the function\'s return \
                        type is %s" (string_of_typ body_typ) 
                 (string_of_typ result_typ))
      | _ -> typ_error p "invalid type annotation on a function"
    end

and tc_exps env es = map (tc_exp env) es

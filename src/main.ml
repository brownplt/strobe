open JavaScript
open Exprjs
open Prelude
open Printf
open Sb_typing
open Format
open Typedjs_syntax
open Typedjs_fromExpr
open Typedjs_env
open Exprjs_syntax
open Format
open FormatExt
open Lexing

module Unidl = Unidl.Make (P) (TypImpl)

let parse_sb cin name =
  let lexbuf = Lexing.from_string cin in
    try
      (* Set the correct filename in lexbuf (for source-tracking). *)
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      Sb_parser.prog Sb_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "lexical error at %s"
                       (Pos.rangeToString
                          lexbuf.lex_curr_p lexbuf.lex_curr_p))
      | Sb_parser.Error ->
           failwith (sprintf "parse error at %s; unexpected token %s"
                       (Pos.rangeToString
                          lexbuf.lex_curr_p lexbuf.lex_curr_p)
                       (lexeme lexbuf))

let string_of_cin cin =
  let buf = Buffer.create 5000 in
    Buffer.add_channel buf cin (in_channel_length cin);
    Buffer.contents buf

let mk_flag flag desc (default : bool) =
  let value = ref default in
  let set_flag () = value := not default in
  let get_flag () = !value in
  let spec = (flag, Arg.Unit set_flag, desc) in
    (spec, get_flag)

module Input : sig
  val get_cin : unit -> string
  val get_cin_name : unit -> string
  val set_cin : in_channel -> string -> unit
  val get_env : unit -> env
  val set_env : env -> unit
  val load_env : string -> unit
  val set_global_object : string -> unit
  val get_global_object : unit -> string
  val set_re_test_depth : int -> unit
  val get_re_test_depth : unit -> int
  val set_re_test_count : int -> unit
  val get_re_test_count : unit -> int
  val get_sourcetype : unit -> string
  val set_sourcetype : string -> unit -> unit
end = struct

  let env = ref empty_env
  let global_object = ref None

  let re_test_depth = ref None
  let re_test_count = ref None

  let c = ref None
  let str = ref None

  let sourcetype = ref "js"

  let cname = ref "no input specified"

  let get_cin () = match !c with
    | None -> failwith "jst: no input files"
    | Some cin -> cin

  let get_cin_name () = !cname

  let set_cin cin name = match !c with
    | None -> c := Some (string_of_cin cin); cname := name
    | Some _ -> failwith "invalid arguments"

  let load_env (fname : string) : unit =
    env := extend_global_env !env (parse_env (open_in fname) fname)

  let set_global_object cname = match !global_object with
    | None -> global_object := Some cname
    | Some _ -> failwith "jst: global object already specified"

  let set_re_test_depth i = re_test_depth := Some i
  let set_re_test_count i = re_test_count := Some i

  let get_sourcetype () = !sourcetype
  let set_sourcetype (str : string) _ = sourcetype := str

  let get_env () = !env

  let get_global_object () = match !global_object with
    | None -> "Global"
    | Some c -> c

  let set_env new_env = env := new_env

  let get_re_test_depth () = match !re_test_depth with
    | None -> 3
    | Some i -> i

  let get_re_test_count () = match !re_test_count with
    | None -> 100
    | Some i -> i

end

open Input

let (set_simpl_cps, get_simpl_cps) =
  mk_flag "-simplcps" "use simplified, but slower CPS (broken)" false


let action_pretty () : int =
  let prog = parse_javascript (get_cin ()) (get_cin_name ()) in
  JavaScript.Pretty.p_prog prog std_formatter;
  print_newline ();
  0

let action_expr () : int =
  let prog = parse_javascript (get_cin ()) (get_cin_name ()) in
  let e = from_javascript prog in
  Exprjs.Pretty.p_expr e std_formatter; print_newline();
  print_newline();
  Exprjs.Pretty.p_expr (lift_decls e) std_formatter;
  0


let src_js : JavaScript_syntax.prog option ref = ref None

let default_typ : TypImpl.typ option ref = ref None

let assert_typ : TypImpl.typ option ref = ref None

let get_typedjs () =
  let tjs = match get_sourcetype () with
    | "js" ->
      let js = parse_javascript (get_cin ()) (get_cin_name ()) in
      src_js := Some js;
      Typedjs_fromExpr.from_exprjs (get_env ()) (lift_decls (from_javascript js))
    | "sb" ->
        (parse_sb (get_cin ()) (get_cin_name ()))
    | ext -> failwith ("unknown file extension " ^ ext)in
  let (prog, _) = unique_ids tjs in
    Sb_owned.owned_inference prog

let weave_annotations typedjs =
  match !src_js with
    | None -> typedjs
    | Some js ->
      let typ_db = ReadTyps.read_typs js (List.rev !JavaScript_lexer.comments) in
      WeaveAnnotations.weave typ_db typedjs

let weave_assertions typedjs =
  match !assert_typ with
  | None -> typedjs
  | Some typ -> WeaveAnnotations.assert_typ typ typedjs


let full_idl_defs : Full_idl_syntax.definition list ref = ref []
let idl_defs : Idl_syntax.definition list ref = ref []

let rec squash env t = 
  let squash = squash env in
  let open TypImpl in 
  let optsquash t = match t with None -> None | Some t -> Some (squash t) in
  match t with
  | TWith _ ->
    let ret = Typedjs_env.simpl_typ env t in
      (* Printf.eprintf "Simplifying\n%s\nto\n%s\n" (string_of_typ t) (string_of_typ ret); *)
    ret
  | TRef (n, t) -> TRef (n, squash t)
  | TSource (n, t) -> TSource (n, squash t)
  | TSink (n, t) -> TSink (n, squash t)
  | TUnion (n, t1, t2) -> TUnion(n, squash t1, squash t2)
  | TIntersect(n, t1, t2) -> TIntersect(n, squash t1, squash t2)
  | TArrow(args, vararg, ret) -> TArrow(map squash args, optsquash vararg, squash ret)
  | TObject ot -> TObject (mk_obj_typ (map (third3 squash) (fields ot)) (absent_pat ot))
  | TTop
  | TBot -> t
  | TForall(n, id, t, b) -> TForall(n, id, squash t, squash b)
  | TId _ 
  | TRegex _
  | TPrim _ -> t
  | TThis t -> TThis (squash t)
  | TRec(n, id, t) -> TRec(n, id, squash t)
  | TLambda (n, args, t) -> TLambda(n, args, squash t)
  | TApp(t, ts) -> TApp(squash t, map squash ts)
  | TFix(n, id, k, t) -> TFix(n, id, k, squash t)
  | TUninit ty -> ty := optsquash !ty; t
let rec elim_twith env exp = 
  let elim = elim_twith env in match exp with
  | EAssertTyp(p, t, e) -> EAssertTyp(p, squash env t, elim e)
  | EArray(p, es) -> EArray(p, map elim es)
  | EObject (p, props) -> EObject (p, map (second2 elim) props)
  | EId _ -> exp
  | EBracket (p, e1, e2) -> EBracket (p, elim e1, elim e2)
  | EUpdate (p, e1, e2, e3) -> EUpdate (p, elim e1, elim e2, elim e3)
  | EPrefixOp (p, op, e) -> EPrefixOp (p, op, elim e)
  | EInfixOp (p, op, e1, e2) -> EInfixOp (p, op, elim e1, elim e2)
  | EIf (p, e1, e2, e3) -> EIf (p, elim e1, elim e2, elim e3)
  | EApp (p, e, es) -> EApp (p, elim e, map elim es)
  | EFunc (p, xs, info, e) -> EFunc (p, xs, info, elim e)
  | ELet (p, x, e1, e2) -> 
    let e1' = elim e1 in
    let e2' = elim e2 in
    ELet (p, x, e1', e2')
  | ERec (p, binds, e) -> ERec (p, map (third3 elim) binds, elim e)
  | ESeq (p, e1, e2) -> ESeq (p, elim e1, elim e2)
  | ELabel (p, x, e) -> ELabel (p, x, elim e)
  | EBreak (p, x, e) -> EBreak (p, x, elim e)
  | ETryCatch (p, e1, x, e2) -> ETryCatch (p, elim e1, x, elim e2)
  | ETryFinally (p, e1, e2) -> ETryFinally (p, elim e1, elim e2)
  | EThrow (p, e) -> EThrow (p, elim e)
  | ETypecast (p, rt, e) -> ETypecast (p, rt, elim e)
  | ERef (p, k, e) -> ERef (p, k, elim e)
  | EDeref (p, e) -> EDeref (p, elim e)
  | ESetRef (p, e1, e2) -> ESetRef (p, elim e1, elim e2)
  | EParen (p, e) -> EParen (p, elim e)
  | ESubsumption (p, t, e) -> ESubsumption (p, squash env t, elim e)
  | EDowncast (p, t, e) -> EDowncast (p, squash env t, elim e)
  | ETypAbs (p, x, t, e) -> ETypAbs (p, x, squash env t, elim e)
  | ETypApp (p, e, t) -> ETypApp (p, elim e, squash env t)
  | ECheat (p, t, e) -> ECheat (p, squash env t, elim e)
  | EBot _
  | EConst _ -> exp


let actual_data () =
  let env =
    let typ_vars = Unidl.unidl !idl_defs in
    Typedjs_env.set_global_object
      (extend_env IdMap.empty typ_vars (get_env ()))
      (get_global_object ()) in
  let typedjs = get_typedjs () in
  let new_decls = ReadTyps.new_decls (List.rev !JavaScript_lexer.comments) in
  let rec helper recIds env d = match d with
    | EnvType(p, x, t) -> 
      let t' = Sb_desugar.desugar_typ p t in
      let t'' = squash env t' in
      (bind_rec_typ_id x recIds (TypImpl.replace_name (Some x) t'') env)
    | ObjectTrio _ -> extend_global_env env [d]
    | RecBind binds ->
      let ids = List.concat (List.map (fun b -> match b with
        | EnvBind (_, x, _) -> [x]
        | EnvType (_, x, _) -> [x]
        | ObjectTrio(_, (c, _), (p, _), (i, _)) -> [c;p;i]
        | EnvPrim _
        | RecBind _ -> []) binds) in
      List.fold_left (helper ids) env binds
    | _ -> env in
  let env = List.fold_left (helper []) env new_decls in
  set_env env;
  let annot_js = elim_twith env (weave_annotations typedjs) in
  let asserted_js = weave_assertions annot_js in
  (env, asserted_js)

let do_print_env = ref false

let print_env env : unit =
 print_env env std_formatter

let set_print_env () : unit =
 do_print_env := true

let action_pretypecheck () : int =
  let (env, typedjs) = actual_data () in
  if (!do_print_env) then print_env env;
  Typedjs_syntax.Pretty.exp typedjs std_formatter;
  0

let action_tc () : int = timefn "Typechecking" (fun () ->
  let (env, asserted_js) = actual_data () in
  if (!do_print_env) then print_env env;
  let _ = typecheck env !default_typ asserted_js in
  if TypImpl.get_num_typ_errors () > 0 then
    2
  else
    0
) ()

let load_idl_file filename =
  let full_idl = Idl.from_channel (open_in filename) filename in
  full_idl_defs := !full_idl_defs @ full_idl;
  idl_defs := !idl_defs @ Simplify_idl.from_full full_idl

let load_new_idl_file filename =
  let full_idl = Idl.from_channel (open_in filename) filename in
  full_idl_defs := !full_idl_defs @ full_idl

let print_idl () : unit =
  Print_full_idl.print_defs !full_idl_defs


let compile_env () : unit =
  let timefn _ f a = f a in
  full_idl_defs := timefn "Sanitizing" (fun defs -> Sanitize.resolve_partials
    (Sanitize.resolve_typedefs (Sanitize.remove_dupes defs))) (!full_idl_defs);
  (* (match (Sanitize.sanity_check !full_idl_defs) with *)
  (* | [] -> Printf.printf "All native types are properly non-scriptable\n" *)
  (* | ids -> Printf.printf "Found identifiers that are native but not noscript!\n"; *)
  (*   List.iter (fun id -> Printf.printf "  %s\n" id) ids); *)
  (* Print_full_idl.print_defs !full_idl_defs; *)
  let (idlEnv, compsType, qiType) = (timefn "Creating env" Create_env.create_env) (!full_idl_defs) in
  timefn "Binding recursive types" (fun () ->
  set_env (unchecked_bind_typ_ids (ListExt.filter_map (fun (name, _, typ) -> 
    match (P.singleton_string name) with
    | Some name -> Some (name, TypImpl.replace_name (Some name) typ)
    | None -> None) idlEnv) (get_env ()))) ();
  set_env (bind_id "Components" (TypImpl.TSource (Some "Components", compsType)) (get_env ()));
  set_env (bind_typ_id "QueryInterfaceType" qiType (get_env ()))
  (* Printf.printf "****************************************\nDone compiling environment@."; *)
  (* set_env (extend_env idlEnv IdMap.empty (get_env ())) *)

let parseTyp_to_ref typStr typref =
  let open Lexing in
  let start_pos = { pos_fname = "<string>"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  let len = String.length typStr in
  let end_pos = { pos_fname = "<string>"; pos_lnum = 1; pos_bol = len; pos_cnum = len } in
  let pos = Pos.real (start_pos, end_pos) in
  match Typedjs_fromExpr.parse_annotation pos typStr with
  | Typedjs_syntax.ATyp typ ->
    let typ = Sb_desugar.desugar_typ pos typ in
    typref := Some typ
  | _ -> ()

let allow_unbound typStr : unit =
  parseTyp_to_ref typStr default_typ

let assert_typ typStr : unit = 
  parseTyp_to_ref typStr assert_typ

let action = ref action_tc

let is_action_set = ref false

let set_action (thunk : unit -> 'a) (() : unit) : unit =
  if !is_action_set then
    (eprintf "invalid arguments (-help for help)\n"; exit 1)
  else
    (is_action_set := true; action := thunk)

let main () : unit =
  let timefn _ f a = f a in
  Arg.parse
    [ ("-tc", Arg.Unit (set_action action_tc),
       "type-check the source program (default when no options are given)");
      ("-stdin", Arg.Unit (fun () -> set_cin stdin "<stdin>"),
       "read from stdin instead of a file");
      ("-env", Arg.String (fun s -> load_env s),
       "<file> read environment types from <file>");
      ("-global", Arg.String (fun s -> set_global_object s),
       "<class> use <class> as the global object");
      ("-pretty", Arg.Unit (set_action action_pretty),
       "pretty-print JavaScript");
      ("-expr", Arg.Unit (set_action action_expr),
       "simplify JavaScript to exprjs");
      ("-pretc", Arg.Unit (set_action action_pretypecheck),
       "basic well-formedness checks before type-checking and flow-analysis");
      ("-noflows", Arg.Unit disable_flows,
       "disable flow analysis (benchmarks and debugging)");
      ("-allow-unbound", Arg.String allow_unbound,
       "Permit unbound global variables, with default type given by the argument");
      ("-verbose-errors", Arg.Clear TypImpl.Pretty.useNames,
       "Print types structurally, rather than with pretty names");
      ("-assert-typ", Arg.String assert_typ,
       "Assert that all un-annotated assignments have a given type");
      ("-sb", Arg.Unit (set_sourcetype "sb"),
       "Parse strobe source");
      ("-idl", Arg.String load_idl_file,
       "loads an IDL file");
      ("-newidl", Arg.String (timefn "Loading IDL" load_new_idl_file),
       "loads an IDL file");
      ("-print-idl", Arg.Unit print_idl,
       "Print the current IDL data");
      ("-compile-env", Arg.Unit (timefn "Compiling environment" compile_env),
       "Generate environment from IDL");
      ("-print-typ", Arg.String (fun ty ->
        let (typ, _) = lookup_typ_id ty (get_env ()) in
        horz [text ty; text "="; TypImpl.Pretty.typ (TypImpl.replace_name None typ)] std_formatter;
        Format.pp_print_flush std_formatter ()),
       "Print the definition of a single type");
      ("-print-env", Arg.Unit set_print_env,
       "Print the current environment");
      set_simpl_cps;
    ]
    (fun s -> set_cin (open_in s) s)
    "Usage: jst [options] [file]\noptions are:\n"

let cleanup () =
  pp_print_flush std_formatter ();
  pp_print_flush err_formatter ();;

at_exit cleanup;
Printexc.print main ();
try
  exit (!action ());
with
    Failure s ->  eprintf "%s\n" s; exit 3
  | Not_well_formed (p, s) ->
      eprintf "%s not well-formed:\n%s\n" (Pos.toString p) s; exit 2
  | TypImpl.Typ_error (p, s) ->
      eprintf "fatal type error at %s: %s\n" (Pos.toString p) (TypImpl.typ_error_details_to_string s); exit 2
  | Sb_kinding.Kind_error s ->
      eprintf "type error (kinding): %s\n" s; exit 2
  | Sb_desugar.Typ_stx_error s ->
      eprintf "type error (annotation): %s\n" s; exit 2

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
                       (string_of_position
                          (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))
      | Sb_parser.Error ->
           failwith (sprintf "parse error at %s; unexpected token %s"
                       (string_of_position
                          (lexbuf.lex_curr_p, lexbuf.lex_curr_p))
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


let action_pretty () : unit =
  let prog = parse_javascript (get_cin ()) (get_cin_name ()) in
    JavaScript.Pretty.p_prog prog std_formatter;
    print_newline ()

let action_expr () : unit =
  let prog = parse_javascript (get_cin ()) (get_cin_name ()) in
  let e = from_javascript prog in
    Exprjs.Pretty.p_expr e std_formatter

let src_js : JavaScript_syntax.prog option ref = ref None

let get_typedjs () =
  let tjs = match get_sourcetype () with
    | "js" ->
      let js = parse_javascript (get_cin ()) (get_cin_name ()) in
      src_js := Some js;
      Typedjs_fromExpr.from_exprjs (get_env ()) (from_javascript js)
    | "sb" ->
        (parse_sb (get_cin ()) (get_cin_name ()))
    | ext -> failwith ("unknown file extension " ^ ext)in
  let (prog, _) = unique_ids tjs in
    Sb_owned.owned_inference prog

let weave_annotations typedjs =
  match !src_js with
    | None -> typedjs
    | Some js ->
      let typ_db =
        ReadTyps.read_typs js (List.rev !JavaScript_lexer.comments) in
      WeaveAnnotations.weave typ_db typedjs

let action_pretypecheck () : unit =
  let typedjs = weave_annotations (get_typedjs ()) in
    Typedjs_syntax.Pretty.exp typedjs std_formatter

let full_idl_defs : Full_idl_syntax.definition list ref = ref []
let idl_defs : Idl_syntax.definition list ref = ref []

let action_tc () : unit =
  let env =
    let typ_vars = Unidl.unidl !idl_defs in
    Typedjs_env.set_global_object
      (extend_env IdMap.empty typ_vars (get_env ()))
      (get_global_object ()) in
  (* verify_env env; *)
  let _ = typecheck env (weave_annotations (get_typedjs ())) in
  if TypImpl.get_num_typ_errors () > 0 then
    exit 2
  else
    ()

let load_idl_file filename =
  let full_idl = Idl.from_channel (open_in filename) filename in
  full_idl_defs := !full_idl_defs @ full_idl;
  idl_defs := !idl_defs @ Simplify_idl.from_full full_idl

let load_new_idl_file filename =
  let full_idl = Idl.from_channel (open_in filename) filename in
  full_idl_defs := !full_idl_defs @ full_idl

let print_env () : unit =
  print_env (get_env()) std_formatter

let print_idl () : unit =
  Print_full_idl.print_defs !full_idl_defs

let compile_env () : unit =
  full_idl_defs := Sanitize.resolve_partials
    (Sanitize.resolve_typedefs (Sanitize.remove_dupes !full_idl_defs));
  (* (match (Sanitize.sanity_check !full_idl_defs) with *)
  (* | [] -> Printf.printf "All native types are properly non-scriptable\n" *)
  (* | ids -> Printf.printf "Found identifiers that are native but not noscript!\n"; *)
  (*   List.iter (fun id -> Printf.printf "  %s\n" id) ids); *)
  (* Print_full_idl.print_defs !full_idl_defs; *)
  let (idlEnv, compsType) = (Create_env.create_env !full_idl_defs) in
  set_env (bind_recursive_types (ListExt.filter_map (fun (name, _, typ) -> 
    match (P.singleton_string name) with
    | Some name -> Some (name, typ)
    | None -> None) idlEnv) (get_env ()));
  set_env (bind_id "Components" (TypImpl.TSource compsType) (get_env ()))
  (* Printf.printf "****************************************\nDone compiling environment@."; *)
  (* set_env (extend_env idlEnv IdMap.empty (get_env ())) *)


let action = ref action_tc

let is_action_set = ref false

let set_action (thunk : unit -> unit) (() : unit) : unit =
  if !is_action_set then
    (eprintf "invalid arguments (-help for help)\n"; exit 1)
  else
    (is_action_set := true; action := thunk)

let main () : unit =
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
      ("-sb", Arg.Unit (set_sourcetype "sb"),
       "Parse strobe source");
      ("-idl", Arg.String load_idl_file,
       "loads an IDL file");
      ("-newidl", Arg.String load_new_idl_file,
       "loads an IDL file");
      ("-print-idl", Arg.Unit print_idl,
       "Print the current IDL data");
      ("-compile-env", Arg.Unit compile_env,
       "Generate environment from IDL");
      ("-print-env", Arg.Unit print_env,
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
  !action ();
  exit 0
with
    Failure s ->  eprintf "%s\n" s; exit 3
  | Not_well_formed (p, s) ->
      eprintf "%s not well-formed:\n%s\n" (string_of_position p) s; exit 2
  | TypImpl.Typ_error (p, s) ->
      eprintf "fatal type error at %s: %s\n" (string_of_position p) s; exit 2
  | Sb_kinding.Kind_error s ->
      eprintf "type error (kinding): %s\n" s; exit 2
  | Sb_desugar.Typ_stx_error s ->
      eprintf "type error (annotation): %s\n" s; exit 2

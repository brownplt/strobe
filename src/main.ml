open JavaScript
open Exprjs
open Prelude
open Printf
open Typedjs_types
open Typedjs_tc
open Format
open Typedjs_syntax
open Typedjs_fromExpr
open Typedjs_env
open Exprjs_syntax
open Format
open FormatExt
open Typedjs_cf
open Typedjs_cftc
open Lexing

module Lat = Typedjs_lattice
module ZZZ = Gadget

let cin = ref stdin

let cin_name = ref "stdin"

let env = ref Env.empty_env

let get_env () = 
  Env.set_global_object !env "HTMLWindow"

let action_load_file path =
  cin := open_in path;
  cin_name := path

let inferred_annotations = ref []

let action_load_inferred name = 
  let lexbuf = Lexing.from_channel (open_in name) in
    try 
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      inferred_annotations := 
        Typedjs_parser.inferred Typedjs_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "lexical error at %s"
                       (string_of_position 
                          (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))
      | Typedjs_parser.Error ->
           failwith (sprintf "parse error at %s"
                       (string_of_position 
                          (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))



let action_pretty () : unit = 
  let prog = parse_javascript !cin !cin_name in
    print_string (JavaScript_pretty.render_prog prog);
    print_newline ()

let action_contracts () : unit = 
  let prog = parse_javascript !cin !cin_name in
  let prog' = Typedjs_contracts.types_to_contracts prog in
    print_string (JavaScript_pretty.render_prog prog');
    print_newline ()

let action_expr () : unit =
  let prog = parse_javascript !cin !cin_name in
  let e = from_javascript prog in
    print_expr e

let get_typedjs () =
  Typedjs_fromExpr.from_exprjs (get_env ())
    (from_javascript (parse_javascript !cin !cin_name))
    !inferred_annotations

let action_pretypecheck () : unit = 
  let typedjs = get_typedjs () in
    Typedjs_syntax.Pretty.p_def typedjs std_formatter

let action_tc () : unit = 
  let typedjs = get_typedjs () in
  let cpstypedjs = Typedjs_cps.cps typedjs in
  let cf_env =
    Lat.bind "%end" (Lat.singleton RT.Function)
      (Lat.bind "%global" (Lat.singleton RT.Object)
         (Lat.bind "%uncaught-exception" (Lat.singleton RT.Function)
            (cf_env_of_tc_env (get_env ())))) in
    typed_cfa cf_env cpstypedjs;
    let annotated_typedjs = insert_typecasts typedjs in
    let _ = Typedjs_tc.typecheck (get_env ()) annotated_typedjs in
    ()

let action_cps () : unit =
  let typedjs = get_typedjs () in
  let cps = Typedjs_cps.cps typedjs in
    Typedjs_cps.p_cpsexp cps std_formatter

let action_df () : unit =
  let typedjs = get_typedjs () in
  let cpstypedjs = Typedjs_cps.cps typedjs in
  let env =
    Lat.bind "%end" (Lat.singleton RT.Function)
      (Lat.bind "%global" (Lat.singleton RT.Object)
         (Lat.bind "%uncaught-exception" (Lat.singleton RT.Function)
            (cf_env_of_tc_env (get_env ())))) in
    typed_cfa env cpstypedjs;
    let annotated_exp = insert_typecasts typedjs in
      Typedjs_syntax.Pretty.p_def annotated_exp std_formatter ;
      printf "Dataflow analysis successful.\n"

let action = ref action_tc

let is_action_set = ref false

let set_action (thunk : unit -> unit) (() : unit) : unit =
  if !is_action_set then
    (eprintf "invalid arguments (-help for help)\n"; exit 1)
  else 
    (is_action_set := true; action := thunk)

let set_env (fname : string) : unit = 
  env := Env.union !env (mk_env (parse_env (open_in fname) fname))

let set_xml_env (fname : string) : unit =
  env := Env.union !env (Gadget.env_of_html (open_in fname))

let set_string_env (fname : string) : unit =
  env := Env.union !env (Gadget.env_of_strings (open_in fname))

let main () : unit =
  Arg.parse
    [ ("-env", Arg.String set_env, "load the environment from a file");
      ("-xmlenv", Arg.String set_xml_env, 
       "load the environment from a file");
      ("-stringenv", Arg.String set_string_env,
       "add strings in a file to the environment");
      ("-pretty", Arg.Unit (set_action action_pretty),
       "pretty-print JavaScript");
      ("-expr", Arg.Unit (set_action action_expr),
       "simplify JavaScript to exprjs");
      ("-pretc", Arg.Unit (set_action action_pretypecheck),
       "basic well-formedness checks before type-checking and flow-analysis");
      ("-cps", Arg.Unit (set_action action_cps),
       "convert program to CPS");
      ("-df", Arg.Unit (set_action action_df),
       "convert program to ANF, then apply dataflow analysis");
      ("-tc", Arg.Unit (set_action action_tc),
       "type-check (default action)");
      ("-inferred", Arg.String action_load_inferred,
       "load inferred annotations");
      ("-contracts", Arg.Unit (set_action action_contracts),
       "insert contracts") ]
    (fun s -> action_load_file s)
    "Typed JavaScript [action] [path]";;

Printexc.print main ();
let exitcode = begin
  try
    !action (); 0
  with 
      Failure s ->  eprintf "%s\n" s; 3
    | Not_well_formed (p, s) -> 
        eprintf "%s not well-formed:\n%s\n" (string_of_position p) s; 2
    | Typ_error (p, s) ->
        eprintf "%s type error:\n%s\n" (string_of_position p) s; 2
end in
  pp_print_flush std_formatter ();
  pp_print_flush err_formatter ();
  exit exitcode

open JavaScript
open Exprjs
open Typedjs
open Prelude
open Printf
open Typedjs_types
open Typedjs_tc
open Typedjs_testing
open Format
open Typedjs_syntax
open Typedjs_pretty
open Typedjs_fromExpr
open Typedjs_env
open Lambdajs_cfa
open Lambdajs_cps
open Exprjs_syntax
open Format
open FormatExt
open Typedjs_cf
open Typedjs_cftc

module Lat = Typedjs_lattice

let cin = ref stdin

let cin_name = ref "stdin"

let env = ref Env.empty_env

let action_load_file path =
  cin := open_in path;
  cin_name := path


let action_pretty () : unit = 
  let (prog, _) = parse_javascript !cin !cin_name in
    print_string (JavaScript_pretty.render_prog prog);
    print_newline ()

let action_comments () : unit = 
    let (_, comments) = parse_javascript !cin !cin_name in
    let pc (pos, str) = printf "%s %s\n" (string_of_position pos) str in
      List.iter pc comments

let action_expr () : unit =
  let (prog, _) = parse_javascript !cin !cin_name in
  let e = from_javascript prog in
    print_expr e

let action_pretypecheck () : unit = 
  let (js, comments) = parse_javascript !cin !cin_name in
  let exprjs = from_javascript js in
  let typedjs = Typedjs.from_exprjs exprjs comments !env in
    Typedjs_pretty.pretty_def std_formatter typedjs

let action_tc () : unit = 
  let (js, comments) = parse_javascript !cin !cin_name in
  let exprjs = from_javascript js in
  let typedjs = Typedjs.from_exprjs exprjs comments !env in
  let cpstypedjs = Typedjs_cps.cps typedjs in
  let cf_env =
    Lat.bind "%end" (Lat.singleton RT.Function)
      (Lat.bind "%uncaught-exception" (Lat.singleton RT.Function)
         Typedjs_lattice.empty_env) in
    typed_cfa cf_env cpstypedjs;
    let annotated_typedjs = insert_typecasts typedjs in
    let _ = Typedjs_tc.typecheck !env annotated_typedjs in
    ()

let action_cps () : unit =
  let (js, comments) = parse_javascript !cin !cin_name in
  let exprjs = from_javascript js in
  let typedjs = Typedjs.from_exprjs exprjs comments !env in
  let cps = Typedjs_cps.cps typedjs in
    Typedjs_cps.p_cpsexp cps std_formatter


let action_esc () : unit =
  let (js, comments) = parse_javascript !cin !cin_name in
  let exprjs = from_javascript js in
  let typedjs = Typedjs.from_exprjs exprjs comments !env in
  let cpstypedjs = Typedjs_cps.cps typedjs in
  let esc = Typedjs_cps.esc_cpsexp cpstypedjs in
    IdSetExt.pretty std_formatter Format.pp_print_string esc


let action_df () : unit =
  let (js, comments) = parse_javascript !cin !cin_name in
  let exprjs = from_javascript js in
  let typedjs = Typedjs.from_exprjs exprjs comments !env in
  let cpstypedjs = Typedjs_cps.cps typedjs in
  let env =
    Lat.bind "%end" (Lat.singleton RT.Function)
      (Lat.bind "%uncaught-exception" (Lat.singleton RT.Function)
         Typedjs_lattice.empty_env) in
    typed_cfa env cpstypedjs;
    let annotated_exp = insert_typecasts typedjs in
      Typedjs_pretty.pretty_def std_formatter annotated_exp

let action_test_tc () : unit =
  Typedjs_testing.parse_and_test !cin !cin_name
   
let action = ref action_tc

let is_action_set = ref false

let set_action (thunk : unit -> unit) (() : unit) : unit =
  if !is_action_set then
    (eprintf "invalid arguments (-help for help)\n"; exit 1)
  else 
    (is_action_set := true; action := thunk)

let set_env (fname : string) : unit = 
  env := mk_env (parse_env (open_in fname) fname)

let main () : unit =
  Arg.parse
    [ ("-env", Arg.String set_env, "load the environment from a file");
      ("-pretty", Arg.Unit (set_action action_pretty),
       "pretty-print JavaScript");
      ("-comments", Arg.Unit (set_action action_comments),
       "extract comments from JavaScript");
      ("-expr", Arg.Unit (set_action action_expr),
       "simplify JavaScript to exprjs");
      ("-pretc", Arg.Unit (set_action action_pretypecheck),
       "basic well-formedness checks before typing");
      ("-cps", Arg.Unit (set_action action_cps),
       "convert program to CPS");
      ("-esc", Arg.Unit (set_action action_esc),
       "escape analysis of CPS");
      ("-df", Arg.Unit (set_action action_df),
       "convert program to ANF, then apply dataflow analysis");
      ("-unittest", Arg.Unit (set_action action_test_tc),
       "(undocumented)");
      ("-tc", Arg.Unit (set_action action_tc),
       "type-check (default action)") ]
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

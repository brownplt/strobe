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

module Input : sig
  val get_cin : unit -> in_channel
  val get_cin_name : unit -> string
  val set_cin : in_channel -> string -> unit
  val get_env : unit -> Env.env
  val load_env : string -> unit
  val set_global_object : string -> unit
end = struct

  let env = ref Env.empty_env
  let global_object = ref None

  let c = ref None

  let cname = ref "no input specified"

  let get_cin () = match !c with
    | None -> failwith "jst: no input files"
    | Some cin -> cin

  let get_cin_name () = !cname

  let set_cin cin name = match !c with
    | None -> c := Some cin; cname := name
    | Some _ -> failwith "invalid arguments" 

  let load_env (fname : string) : unit = 
    env := extend_global_env !env (parse_env (open_in fname) fname)

  let set_global_object cname = match !global_object with
    | None -> global_object := Some cname
    | Some _ -> failwith "jst: global object already specified"

  let get_env () = match !global_object with
    | None -> Env.set_global_object !env "Global"
    | Some c -> Env.set_global_object !env c

end

open Input

let action_pretty () : unit = 
  let prog = parse_javascript (get_cin ()) (get_cin_name ()) in
    JavaScript.Pretty.p_prog prog std_formatter;
    print_newline ()

let action_contracts () : unit = 
  let prog = parse_javascript (get_cin ()) (get_cin_name ()) in
  let prog' = Typedjs_contracts.types_to_contracts prog in
    JavaScript.Pretty.p_prog prog' std_formatter;
    print_newline ()

let action_expr () : unit =
  let prog = parse_javascript (get_cin ()) (get_cin_name ()) in
  let e = from_javascript prog in
    Exprjs.Pretty.p_expr e std_formatter

let get_typedjs () =
  Typedjs_fromExpr.from_exprjs (get_env ())
    (from_javascript (parse_javascript (get_cin ()) (get_cin_name ())))

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

let main () : unit =
  Arg.parse
    [ ("-tc", Arg.Unit (set_action action_tc),
       "type-check the source program (default when no options are given)");
      ("-contracts", Arg.Unit (set_action action_contracts),
       "insert contracts into the source program") ;
      ("-stdin", Arg.Unit (fun () -> set_cin stdin "<stdin>"),
       "read from stdin instead of a file");
      ("-env", Arg.String (fun s -> load_env s),
       "<file> read environment types from <file>");
      ("-pretty", Arg.Unit (set_action action_pretty),
       "pretty-print JavaScript");
      ("-expr", Arg.Unit (set_action action_expr),
       "simplify JavaScript to exprjs");
      ("-pretc", Arg.Unit (set_action action_pretypecheck),
       "basic well-formedness checks before type-checking and flow-analysis");
      ("-cps", Arg.Unit (set_action action_cps),
       "convert program to CPS");
      ("-df", Arg.Unit (set_action action_df),
       "convert program to ANF, then apply dataflow analysis")
    ]
    (fun s -> set_cin (open_in s) s)
    "Usage: jst [options] [file]\noptions are:\n";;

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

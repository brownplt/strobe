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
open Lexing
open Typedjs_dyn
open RegLang
open RegLang_generate

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
  val get_env : unit -> Env.env
  val load_env : string -> unit
  val set_global_object : string -> unit
  val set_re_test_depth : int -> unit
  val get_re_test_depth : unit -> int
  val set_re_test_count : int -> unit
  val get_re_test_count : unit -> int
end = struct

  let env = ref Env.empty_env
  let global_object = ref None

  let re_test_depth = ref None
  let re_test_count = ref None

  let c = ref None
  let str = ref None

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


  let get_env () = match !global_object with
    | None -> Env.set_global_object !env "Global"
    | Some c -> Env.set_global_object !env c

  let get_re_test_depth () = match !re_test_depth with
    | None -> 3
    | Some i -> i

  let get_re_test_count () = match !re_test_count with
    | None -> 100
    | Some i -> i

end

open Input

let (set_print_contracts, get_print_contracts) =
  mk_flag "-contracts" "insert contracts (prints to stdout)" false

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

let get_typedjs () =
  let (prog, _) = unique_ids 
    (Typedjs_fromExpr.from_exprjs (get_env ())
       (from_javascript (parse_javascript (get_cin ()) (get_cin_name ())))) 
  in Sb_owned.owned_inference prog

let action_pretypecheck () : unit = 
  let typedjs = get_typedjs () in
    Typedjs_syntax.Pretty.p_def typedjs std_formatter

let action_tc () : unit = 
  let _ = Typedjs_tc.typecheck (get_env ()) (get_typedjs ()) in
    if get_print_contracts () then
      let tr_map = mk_contract_transformers !contracts in
        transform_exprs tr_map (get_cin ()) stdout

let action_reglang () : unit =
  let depth = get_re_test_depth () in
  let count = get_re_test_count () in
  let res1 = random_res depth count in
  let res2 = random_res depth count in
  List.iter2 (fun re1 re2 -> 
    printf "%s <: %s;\n" (RegLang_syntax.Pretty.string_of_re re1)
      (RegLang_syntax.Pretty.string_of_re re2))
    res1 res2
  

let action_regex () : unit =
  printf "Regexing\n";
  let lexbuf = from_string (get_cin ()) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = get_cin_name () };
  let tests = 
    try
      RegLang_parser.regex_tests RegLang_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (sprintf "lexical error at %s"
                       (string_of_position
                          (lexbuf.lex_curr_p, lexbuf.lex_curr_p)))
      | RegLang_parser.Error ->
        failwith (sprintf "parse error at %s; unexpected token %s"
                    (string_of_position
                       (lexbuf.lex_curr_p, lexbuf.lex_curr_p))
                    (lexeme lexbuf)) in
  let run_test (pos, re1, re2, should_succeed) = 
    let fsm1 = RegLang.fsm_of_regex re1 in
    let fsm2 = RegLang.fsm_of_regex re2 in
    eprintf "Testing: %s <: %s\n" (RegLang_syntax.Pretty.string_of_re re1) 
      (RegLang_syntax.Pretty.string_of_re re2);
      match RegLang.counterexample fsm1 fsm2, should_succeed with
        | None, false -> eprintf "(Failed) Found no overlap, but expected to\n"; ()
        | Some str, true -> eprintf "(Failed) Found overlap: %s\n" str; () 
        | _, _ -> eprintf "Done\n%!"; () in
  List.iter run_test tests
  

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
      ("-disable-unreachable", Arg.Unit Typedjs_tc.disable_unreachable_check,
       "do not signal an error on unreachable code");
      ("-noflows", Arg.Unit Typedjs_tc.disable_flows,
       "disable flow analysis (benchmarks and debugging)");
      ("-regex", Arg.Unit (set_action action_regex),
       "regular expression containment tests");
      ("-regex-depth", Arg.Int (fun i -> set_re_test_depth i),
       "set the depth of the res to generate (use with regex-generate)");
      ("-regex-generate", Arg.Int (fun i -> set_re_test_count i;
        (set_action action_reglang) ()),
       "generate <count> random regular expressions");
      set_simpl_cps;
      set_print_contracts;

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

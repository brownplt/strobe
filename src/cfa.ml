open JavaScript
open Exprjs
open Typedjs
open Prelude
open Printf
open Format
open Lambdajs_cfa
open Lambdajs_cps
open Exprjs_syntax
open Format
open FormatExt
open Lambdajs_lattice

module H = Hashtbl

let empty_vars_at node cpsexp = 
  let vars = fv_immediate cpsexp in
  let env = H.find envs node in
  let find_var x = 
    try 
      ignore (IdMap.find x env)
    with Not_found -> printf "No values for %s at %d.\n" x node in
   IdSet.iter find_var vars

let empty_vars () = 
  Hashtbl.iter empty_vars_at reachable



let cin = ref stdin

let cin_name = ref "stdin"

let action_load_file path =
  cin := open_in path;
  cin_name := path

let action_cps () : unit =
  let (js, comments) = parse_javascript !cin !cin_name in
  let exprjs = from_javascript js in
  let lambdajs = Lambdajs_syntax.desugar exprjs in
  let cpslambdajs = Lambdajs_cps.cps lambdajs in
    Lambdajs_cps.p_cpsexp cpslambdajs std_formatter

let action_cfa () : unit =
  let (js, comments) = parse_javascript !cin !cin_name in
  let exprjs = from_javascript js in
  let lambdajs = Lambdajs_syntax.desugar exprjs in
  let cpsexp = Lambdajs_cps.cps lambdajs in
  let print_env node (env : AV.env)  = 
    printf "Node: %d %s\n" node 
      (to_string (IdMapExt.p_map text (AVSetExt.p_set p_av)) env)
  in Lambdajs_cfa.cfa cpsexp;
    Hashtbl.iter print_env envs;
    empty_vars ()


let action = ref action_cps

let is_action_set = ref false

let set_action (thunk : unit -> unit) (() : unit) : unit =
  if !is_action_set then
    (eprintf "invalid arguments (-help for help)\n"; exit 1)
  else 
    (is_action_set := true; action := thunk)





let main () : unit =
  Arg.parse
    [ ("-cps", Arg.Unit (set_action action_cps),
       "convert program to CPS");
      ("-cfa", Arg.Unit (set_action action_cfa),
       "(undocumented)")
    ]
    (fun s -> action_load_file s)
    "Typed JavaScript [action] [path]";;

Printexc.print main ();
Printexc.print !action ()

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


let overlay_call_graph coords : unit =
  let arrow (from_node : int) (to_node : int) : unit = 
    let (x1, y1) = H.find coords from_node in
    let (x2, y2) = H.find coords to_node in
      fprintf std_formatter "<path d=\"M %d %d L %d %d\">\n"
        (x1 * 12) (y1 * 20) (x2 * 12) (y2 * 20) in
  let arrows_from (from_node : int) (to_set : IntSet.t) : unit  = 
    IntSet.iter (arrow from_node) to_set in

    H.iter arrows_from call_graph
    


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
    use_svg_formatter ();
    Lambdajs_cps.p_cpsexp cpslambdajs std_formatter;
    Format.pp_print_flush std_formatter ()
      

let action_cfa () : unit =
  let (js, comments) = parse_javascript !cin !cin_name in
  let exprjs = from_javascript js in
  let lambdajs = Lambdajs_syntax.desugar exprjs in
  let cpsexp = Lambdajs_cps.cps lambdajs in
    Lambdajs_cfa.cfa cpsexp;
    use_svg_formatter ();
    Lambdajs_cps.p_cpsexp cpsexp std_formatter;
    use_std_formatter ()



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

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


(* [find_coords svg_str] determines the coordinates of nodes in an SVG
   document. *)
let find_coords (svg_str : string) : (int, int * int) H.t =
  let re = Str.regexp "x=\"\\([0-9]+\\)\" y=\"\\([0-9]+\\)\">\
                       \\(app\\|let\\|fix\\|if\\)/\\([0-9]+\\)"
  in 

  let matches = H.create 100 in
  let rec find start_pos = begin
    try 
      let _ = Str.search_forward  re svg_str start_pos in
      let x = int_of_string (Str.matched_group 1 svg_str) in
      let y = int_of_string (Str.matched_group 2 svg_str) in
      let node = int_of_string (Str.matched_group 4 svg_str) in
        eprintf "Node %d is at (%d, %d)\n" node x y;
        H.add matches node (x,y);
        find (Str.match_end ())
    with Not_found -> () 
  end in
    find 0;
    matches
    

(*
let empty_vars_at node cpsexp = 
  let vars = fv_immediate cpsexp in
  let env = H.find envs node in
  let find_var x = lookup x env in
   IdSet.iter find_var vars

let empty_vars () = 
  Hashtbl.iter empty_vars_at reachable
*)


let overlay_call_graph coords : unit =
  let arrow (from_node : int) (to_node : int) : unit = 
    eprintf "Drawing edge from %d to %d\n" from_node to_node;
    try 
      let (x1, y1) = H.find coords from_node in
      let (x2, y2) = H.find coords to_node in
        fprintf std_formatter 
          "<path d=\"M %d %d L %d %d\" 
                 style=\"stroke-width:1px; stroke:#0000ff; \
                        marker-end:url(#Arrow2Lend)\"></path>\n"
          x1 y1 x2 y2
    with Not_found -> () in
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
    Lambdajs_cps.p_cpsexp cpslambdajs std_formatter
      

let action_cfa () : unit =
  let (js, comments) = parse_javascript !cin !cin_name in
  let exprjs = from_javascript js in
  let lambdajs = Lambdajs_syntax.desugar exprjs in
  let cpsexp = Lambdajs_cps.cps lambdajs in
    Lambdajs_cfa.cfa cpsexp;
    Lambdajs_cps.p_cpsexp cpsexp svg_formatter;
    let src = flush_svg_formatter () in
      print_string src;
      overlay_call_graph (find_coords src);
      print_string "</svg>";
      let pr_env node env =
        eprintf "\nEnv at node %d is\n %s\n" node (to_string p_env env) in
        Hashtbl.iter pr_env envs
        
        


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

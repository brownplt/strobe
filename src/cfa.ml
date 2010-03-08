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
module ZZ = Lambdajs_lexer

let print_env_at cxt = 
  let env = H.find envs cxt in
    p_env env std_formatter

let print_heap_at cxt =
  let sto = H.find heaps cxt in
    p_heap sto std_formatter

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

let verify_app node exp = match exp with
  | App (_, Id x, _) ->
      let v = lookup x (Hashtbl.find envs node) in
      let set = Range.up (Type.up (Hashtbl.find heaps node) v) in
        if AVSet.is_empty set then
          eprintf "Unapplied application at %d.\n" node
        else
          ()
  | If (_, Id x, _, _) ->
      let v = lookup x (Hashtbl.find envs node) in
      let set = Range.up (Type.up (Hashtbl.find heaps node) v) in
        if AVSet.is_empty set then
          eprintf "Branch skipped at %d.\n" node
        else
          ()
  | Bind ((n, _), x, e, cont) ->
      let bound_node = cpsexp_idx cont in
      let v = lookup x (Hashtbl.find envs bound_node) in
      let set = Range.up (Type.up (Hashtbl.find heaps node) v) in
        if AVSet.is_empty set then
          begin
            eprintf "let/%d %s = %s is empty\n" node x
              (FormatExt.to_string Pretty.p_bindexp e)
          end 
        else
          ()            
  | _ -> ()
      


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
      Hashtbl.iter verify_app reachable;
      eprintf "%d nodes reached out of %d total nodes.\n"
        (Hashtbl.length reachable) (Cps.num_nodes ())
      ;print_env_at 20;
      pp_print_newline std_formatter ();
      print_heap_at 20


let action_cps_lambdajs () : unit =
  let lambdajs = Lambdajs.parse_lambdajs !cin !cin_name in
  let cpslambdajs = Lambdajs_cps.cps lambdajs in
    Lambdajs_cps.p_cpsexp cpslambdajs std_formatter

open Lambdajs_env
open Lambdajs_syntax
open Lexing

let action_env () : unit =
  let env = parse_env !cin !cin_name in
  let exp = enclose_in_env env (EConst ((dummy_pos, dummy_pos), CUndefined)) in
  let fvs = fv exp in
    if not (IdSet.is_empty fvs) then
      printf "Unbound identifiers in environment: %s\n"
        (to_string (IdSetExt.p_set text) fvs)

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
       "(undocumented)");
      ("-testcps", Arg.Unit (set_action action_cps_lambdajs),
       "(undocumented)");
      ("-env", Arg.Unit (set_action action_env),
       "(undocumented)")
    ]
    (fun s -> action_load_file s)
    "Typed JavaScript [action] [path]";;

main ();
!action ()

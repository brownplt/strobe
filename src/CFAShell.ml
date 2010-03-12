open Prelude
open Lambdajs_cps
open FormatExt
open Format
open Scanf

type curr = {
  curr_exp : cpsexp;
  parent : curr option;
}
  

let rec enclosing_branch(curr : curr) = match curr.curr_exp with
  | If (_, _, _, _) -> Some (curr.curr_exp, curr.parent)
  | _ -> match curr.parent with
      | None -> None
      | Some p -> enclosing_branch  p


let rec p_simple_lambda (f, args, body) =
  horz [ text f; text "="; p_simple_exp body ]

and p_simple_exp exp = match exp with
  | Fix ((n, _), binds, _) -> 
      horz [ text (string_of_int n ^ "/fix");
             vert (map p_simple_lambda binds) ]
  | App ((n, _), f, args) ->
      parens [ horz [ text (string_of_int n ^ "/app"); Pretty.p_cpsval f; 
                      horz (map Pretty.p_cpsval args) ] ]
  | If ((n, _), v, _, _) ->
      parens [ horz [ text (string_of_int n ^ "/if"); Pretty.p_cpsval v; 
                      text "..."; text "..." ] ]
  | Bind ((n, _), x, e, _) ->
      horz [ text (string_of_int n ^ "/let"); text x; text "=";
             Pretty.p_bindexp e; text "in"; text "..." ]

let rec do_action (cmd : string) (curr : curr) : curr = match cmd with
  | "p" 
  | "print" -> p_simple_exp curr.curr_exp std_formatter; curr
  | "d"
  | "down" -> begin match curr.curr_exp with
      | Fix (_, _, k) -> 
          do_action "print" { curr_exp = k; parent = Some curr }
      | If _ ->
          printf "Use true/false to navigate an if expression.\n@?";
          curr
      | App _ ->
          printf "Nothing follows applications.\n@?";
          curr
      | Bind (_, _, _, k) ->
          do_action "print" { curr_exp = k; parent = Some curr }
    end
  | "t" | "true" -> begin match curr.curr_exp with
      | If (_, _, t, f) -> 
          do_action "print" { curr_exp = t; parent = Some curr }
      | _ -> printf "Not at a branch.\n@?"; curr
    end 
  | "f" | "false" -> begin match curr.curr_exp with
      | If (_, _, _, f) -> 
          do_action "print" { curr_exp = f; parent = Some curr }
      | _ -> printf "Not at a branch.\n@?"; curr
    end 
  | "u" | "up" -> begin match curr.parent with
      | None -> printf "At a root.\n"; curr
      | Some c -> do_action "print" c
    end
  | "" | "n" | "next" -> begin match curr.curr_exp with
      | Fix (_, _, k) -> 
          printf "down\n";
          do_action "print" { curr_exp = k; parent = Some curr }
      | If (_, _, t, f) ->
          printf "true\n";
          do_action "print" 
            { curr_exp = t; parent = Some curr }
      | App _ -> begin match enclosing_branch curr with
          | Some (If (_, _, _, f), parent) -> 
              do_action "print" { curr_exp  = f; parent =  parent }
          | Some _ -> 
              printf "ERROR: enclosing_branch should return an If\n";
              curr
          | None -> printf "Last expression.\n"; curr
        end
      | Bind (_, _, _, k) ->
          do_action "print" { curr_exp = k; parent = Some curr }
    end 
  | _ -> eprintf "Unknown command.\n@?"; curr




let rec cmd_loop e : unit = 
    printf "\nCommand: @?";
  let cmd = read_line () in

  sscanf cmd "%s" 
    (fun cmd -> cmd_loop (do_action cmd e))

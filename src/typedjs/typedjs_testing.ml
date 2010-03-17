open Prelude
open Typedjs_testLexer
open Typedjs_pretty
open Typedjs_tc
open Typedjs_types
open Typedjs_syntax
open Typedjs_env
open Typedjs_fromExpr

let string_of_typ = pretty_string pretty_typ

(* It would better to use a state monad for this. Since the test function
   should not modify num_tests, we should have 

   parse_and_test :: Channel -> String -> StateT Int (State Int) ()
   test :: State Int ()

   and use 'lift test' to apply test in parse_and_test. *)
let num_tests = ref 0

let num_failures = ref 0

let test ((pos, js_expr, comments, expected) : test) : unit =
  try
    let expr_stx = Exprjs_syntax.from_javascript_expr js_expr in
    let typed_stx = begin match Typedjs.from_exprjs expr_stx comments
      (Env.empty_env) with
         DExp (e, DEnd) -> e
      | _ -> 
          failwith (sprintf "@%s: expected a single expression; got other \
                             definitions"  (string_of_position pos))
    end in
    let actual_typ = tc_exp Env.empty_env typed_stx in
      begin match expected with
          ExpectedTyp t ->
            if subtype IdMap.empty actual_typ t && 
              subtype IdMap.empty t actual_typ then 
              ()
            else begin
              incr num_failures;
              printf "@%s:\n expected type %s, actual type %s\n" 
                (string_of_position pos) (string_of_typ t)
                (string_of_typ actual_typ)
            end
        | ExpectedFails ->
            incr num_failures;
            printf "@%s:\n expected failure, succeeded with type %s\n" 
              (string_of_position pos) (string_of_typ actual_typ)
      end
  with Failure s -> 
    begin match expected with
        ExpectedFails -> ()
      | ExpectedTyp t -> 
          incr num_failures;
          printf "@%s:\n expected type %s, failed with reason: %s\n"
            (string_of_position pos) (string_of_typ t) s
    end
    | Not_well_formed (p, s) ->
        begin match expected with
            ExpectedFails -> ()
          | ExpectedTyp t -> 
              incr num_failures;
              printf "@%s:\n expected type %s, failed (Not_well_formed): %s\n"
                (string_of_position pos) (string_of_typ t) s
        end

let parse_and_test cin name = 
  let tests = parse_tests cin name in
    num_failures := 0;
    num_tests := 0;
    List.iter (fun x -> incr num_tests; test x) tests;
    printf "Testing complete on %s\n%d total tests, %d tests failed.\n"
      name !num_tests !num_failures
    

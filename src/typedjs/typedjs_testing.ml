open Prelude
open Typedjs_testLexer
open Typedjs_pretty
open Typedjs_tc
open Typedjs_types

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
    let typed_stx = Typedjs.from_exprjs expr_stx comments in
    let actual_typ = tc_exp Env.empty_env typed_stx in
      begin match expected with
          ExpectedTyp t ->
            if subtype actual_typ t && subtype t actual_typ then 
              ()
            else begin
              incr num_failures;
              printf "@%s:\n expected type %s, actual type %s\n" 
                (string_of_position pos) (string_of_typ t)
                (string_of_typ actual_typ)
            end
        | ExpectedFails ->
            printf "@%s:\n expected failure, succeeded with type %s\n" 
              (string_of_position pos) (string_of_typ actual_typ)
      end
  with Failure s -> match expected with
      ExpectedFails -> ()
    | ExpectedTyp t -> 
        incr num_failures;
        printf "@%s:\n expected type %s, failed with reason: %s\n"
          (string_of_position pos) (string_of_typ t) s

let parse_and_test cin name = 
  let tests = parse_tests cin name in
    num_failures := 0;
    num_tests := 0;
    List.iter (fun x -> incr num_tests; test x) tests;
    printf "Testing complete on %s\n%d total tests, %d tests failed.\n"
      name !num_tests !num_failures
    

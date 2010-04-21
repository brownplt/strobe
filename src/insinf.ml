(** Splices a list of type annotations onto JavaScript functions, turning it
    into Typed JavaScript! *)
open Str

module Main = struct

let rec output_rest cin cout = 
  try
    output_string cout (input_line cin);
    output_char cout '\n';
    output_rest cin cout
  with End_of_file -> ()

let rec input_strings cin : string list = 
  try let s = input_line cin in
    s :: (input_strings cin)
  with End_of_file -> []

let function_re : regexp = regexp ".*function[^(]*([^)]*)"

let rec splice_typ (cin : in_channel) (cout : out_channel) (typ : string) = 
  let line = input_line cin in
    if string_match function_re line 0 then
      begin
        let p = match_end () in (* first character after argument list *)
          output cout line 0 p;
          output_string cout " /*: ";
          output_string cout typ;
          output_string cout " */";
          output cout line p (String.length line - p);
          output_char cout '\n'
      end
    else
      begin
        output_string cout line;
        output_char cout '\n';
        splice_typ cin cout typ
      end

let splice_typs cin cout typs = 
  List.iter (splice_typ cin cout) typs;
  output_rest cin cout

let main () : unit =
  splice_typs stdin stdout (input_strings (open_in (Sys.argv.(1))))

end;;

Main.main ()

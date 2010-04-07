(** Splices a list of type annotations onto JavaScript functions, turning it
    into Typed JavaScript! *)

open Prelude
open Str

module Main = struct

let rec output_rest cin cout = 
  try
    output_string cout (input_line cin);
    output_rest cin cout
  with End_of_file -> ()

let rec input_strings cin : string list = 
  try let s = input_line cin in
    s :: (input_strings cin)
  with End_of_file -> []

(** Group \1 is the whitespace beween the close-paren of the argument list and
    the open-bracket of the function body. *)
let function_re : regexp = regexp "function.*(.*)\\([ ]*\\){"

let rec splice_typ (cin : in_channel) (cout : out_channel) (typ : string) = 
  let line = input_line cin in
    if string_match function_re line 0 then
      begin
        let p = group_beginning 1 in (* first character after close-paren *)
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

let typs = ref []

let read_typs typ_file = 
  typs := input_strings (open_in typ_file)

let main () : unit =
  Arg.parse [ ("-typs", Arg.String read_typs, "type annotation file") ]
    (fun _ -> failwith "extraneous arguments")
    "jst-insinf -typ PATH < SRC > DEST";
  splice_typs stdin stdout !typs

end;;

Main.main ()

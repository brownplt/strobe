open Prelude
open RegLang
open RegLang_syntax
open Random

let max_string_length = 10

let random_char () = Char.chr ((Random.int 26) + 97)

let random_string () = 
  let len = 1 + (Random.int max_string_length) in
  let rec f chars_left = match chars_left with
    | 0 -> ""
    | n -> (Char.escaped (random_char ())) ^ (f (n - 1)) in
  f len
  
let generate depth =
  let rec f depth =
    match Random.int 6 with
      | 0 -> Concat (f (depth - 1), f (depth - 1))
      | 1 -> Alt (f (depth - 1), f (depth - 1))
      | 2 -> Star (f (depth - 1))
      | 3 -> String (random_string ())
      | 4 -> InSet (CharSet.singleton (random_char ()))
      | 5 -> NotInSet (CharSet.singleton (random_char ()))
      | _ -> failwith "Horrific error --- ocaml's random broke" in
  f depth
    
let random_re depth = generate depth

let random_res depth how_many = 
  Random.self_init ();
  let rec f how_many' = match how_many' with
    | 0 -> []
    | n -> (generate depth)::(f (how_many' - 1)) in
  f how_many

type t = Str of string | Num of int

let next_name = ref 0

let gen_id () = 
  incr next_name;
  Num (!next_name - 1)

let id_of_string x = Str x

let string_of_id t = match t with
  | Num n -> "#" ^ string_of_int n
  | Str s -> s

let compare = Pervasives.compare

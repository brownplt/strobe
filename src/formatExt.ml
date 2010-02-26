open Format

type printer = formatter -> unit
 
let nest (p : printer) (fmt : formatter) : unit =
  pp_open_vbox fmt 2;
  p fmt;
  pp_close_box fmt ()
 
let rec sep (lst : printer list) (fmt : formatter) : unit = match lst with
    x1 :: x2 :: xs' ->
      pp_open_box fmt 2;
      x1 fmt; 
      pp_close_box fmt ();
      pp_print_space fmt (); 
      sep (x2 :: xs') fmt
  | [x] -> 
      pp_open_box fmt 2;
      x fmt;
      pp_close_box fmt ()
  | [] -> ()
 
 
let vert (p : printer list) (fmt : formatter) : unit = 
  pp_open_vbox fmt 0;
  sep p fmt;
  pp_close_box fmt ()
 
let horz (p : printer list) (fmt : formatter) : unit = 
  pp_open_hbox fmt ();
  sep p fmt;
  pp_close_box fmt ()
  
let text s fmt = pp_print_string fmt s
 
let int n fmt = pp_print_int fmt n
 
let enclose l r (lst : printer list) (fmt : formatter) = 
  pp_open_box fmt 2;
  pp_print_string fmt l;
  sep lst fmt;    
  pp_print_string fmt r;
  pp_close_box fmt ()
 
let parens = enclose "(" ")"
 
let braces = enclose "{" "}"
 
let brackets = enclose "[" "]"

let angles = enclose "<" ">"

let to_string (f : 'a -> printer) (x : 'a) : string  =
  f x str_formatter;
  flush_str_formatter ()


let ps = Pervasives.print_string
let pi = Pervasives.print_int
let pn = Pervasives.print_newline


let xml_escape (raw_str : string) : string = 
  let buf = Buffer.create (String.length raw_str) in
  let copy_char ch = match ch with
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | _ -> Buffer.add_char buf ch in
    String.iter copy_char raw_str;
    Buffer.contents buf
    

let svg_line = ref 0
let svg_col = ref 0

let use_std_formatter : unit -> unit =
  let (out, flush, newline, spaces) = get_all_formatter_output_functions () in
    fun () ->
      set_all_formatter_output_functions out flush newline spaces

let use_svg_formatter () : unit =
  ps "<svg xmlns=\"http://www.w3.org/2000/svg\" \
           xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n";
  let out src_str start_pos num_chars = 
    let out_str = xml_escape (String.sub src_str start_pos num_chars) in
      ps "<text style=\"font-family: courier\" x =\""; 
      pi (!svg_col * 12);  ps "\" y = \""; 
        pi (!svg_line * 20); ps "\">";
      ps out_str;
      ps "</text>";
      pn ();
      svg_col := !svg_col + num_chars
  and flush () = () 
  and newline () = 
    incr svg_line;
    svg_col := 0 
  and spaces n =
    svg_col := !svg_col + n
  in set_all_formatter_output_functions out flush newline spaces

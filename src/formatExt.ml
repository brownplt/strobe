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

let rec squish (lst : printer list) (fmt : formatter) : unit = match lst with
  | x :: xs -> x fmt; squish xs fmt
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

let set_html_formatter_tag_functions () : unit = 
  let ftf = get_formatter_tag_functions () in
  let re = Str.regexp "\\([a-z]+\\) .*" in
  let close_tag tag =
    let tag' = try 
      let _ = Str.search_forward re tag 0 in
        Str.matched_group 1 tag
    with 
      | Not_found -> (eprintf "Error parsing tag %s" tag; tag) in
      ("</" ^ tag' ^ ">") in
    set_formatter_tag_functions { ftf with mark_close_tag = close_tag }


let tag (s : string) (p : printer) (f : formatter) : unit = 
  pp_open_tag f s;
  p f;
  pp_close_tag f ()

(*
let mark (s : string) (f : formatter) : unit =
  let 
val mark : string -> printer *)

let to_string (f : 'a -> printer) (x : 'a) : string  =
  f x str_formatter;
  flush_str_formatter ()

let xml_escape (raw_str : string) : string = 
  let buf = Buffer.create (String.length raw_str) in
  let copy_char ch = match ch with
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | _ -> Buffer.add_char buf ch in
    String.iter copy_char raw_str;
    Buffer.contents buf
    
let (flush_svg_formatter, svg_formatter) = 
  let buf = Buffer.create 5000 in
  let formatter = formatter_of_buffer buf in
  let (out, flush, newline, spaces) = 
    pp_get_all_formatter_output_functions formatter () in
  let svg_line = ref 0 in
  let svg_col = ref 0 in
  let max_svg_col = ref 0 in
  let tmp_buf = Buffer.create 100 in

  let out' src_str start_pos num_chars = 
    Buffer.add_string tmp_buf 
      (sprintf "<text style=\"font-family: courier\" x=\"%d\" y=\"%d\">"
         (!svg_col * 10)  (!svg_line * 20));
    Buffer.add_string tmp_buf 
      (xml_escape (String.sub src_str start_pos num_chars));
    Buffer.add_string tmp_buf "</text>\n";
    svg_col := !svg_col + num_chars;
    out (Buffer.contents tmp_buf) 0 (Buffer.length tmp_buf);
    Buffer.clear tmp_buf
  and newline' () = 
    incr svg_line;
    max_svg_col := max !max_svg_col !svg_col;
    svg_col := 0;
    newline ()
  and spaces' n =
    svg_col := !svg_col + n;
    spaces n in
    Buffer.add_string buf
      "<defs><marker \
       inkscape:stockid=\"Arrow2Lend\" \
       orient=\"auto\" \
       refY=\"0.0\" \
       refX=\"0.0\" \
       id=\"Arrow2Lend\" \
       style=\"overflow:visible;\"> \
       <path
         style=\"font-size:12.0;fill-rule:evenodd;stroke-width:0.62500000; \
                 stroke-linejoin:round;\" \
         d=\"M 8.7185878,4.0337352 L -2.2072895,0.016013256 L 8.7185884, \
            -4.0017078 C 6.9730900,-1.6296469 6.9831476,1.6157441 \
             8.7185878,4.0337352 z \" \
         transform=\"scale(1.1) rotate(180) translate(1,0)\" /> \
      </marker></defs>";
    pp_set_all_formatter_output_functions formatter out' flush newline' spaces';
    (fun () -> pp_print_flush formatter ();
       let out_buf = Buffer.create (Buffer.length buf + 200) in
         Buffer.add_string out_buf
           (sprintf
              "<svg xmlns=\"http://www.w3.org/2000/svg\" \
              xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\" \
              xmlns:xlink=\"http://www.w3.org/1999/xlink\" \
              width=\"%d\" height=\"%d\">\n"
              ((!max_svg_col + 10) * 10)
              ((!svg_line + 10) * 20));
         Buffer.add_buffer out_buf buf;
         Buffer.clear buf;
         Buffer.contents out_buf), formatter

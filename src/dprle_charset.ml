(*  Copyright (c) 2008-2009, University of Virginia
    All rights reserved.
   
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
       * Redistributions of source code must retain the above copyright
         notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
         copyright notice, this list of conditions and the following
         disclaimer in the documentation and/or other materials
         provided with the distribution.
       * Neither the name of the University of Virginia nor the names 
         of its contributors may be used to endorse or promote products
         derived from this software without specific prior written
         permission.
   
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
    OF THE POSSIBILITY OF SUCH DAMAGE.
   
    Author: Pieter Hooimeijer
*)

module Hashset = Dprle_hashset

(** Charsets are just hashsets of integers in the range
    0-255
*)
type set = int Hashset.hashset

let default_set_size = 200 
exception IllegalChar

let valid_char (c : int) : bool =
  c >= 0 && c < 256
      
let char_as_string (c : int) : string =
  Printf.sprintf "\\%03d" c

let string_to_int (c : string) : int =
  if String.length c = 1 then
    Char.code (String.get c 0)
  else
    raise IllegalChar

let digit_list_to_int (dl : string) : int =
  if String.length dl = 3 then
    (try 
       int_of_string dl
     with _ -> raise IllegalChar)
  else raise IllegalChar

let create_empty () : set = 
  Hashset.create 0

let create_full () : set =
  let ret = Hashset.create 256 in
    for i = 0 to 255 do Hashset.add ret i done;
    ret

let copy  : set -> set  = Hashset.copy
let mem   : set -> int -> bool = Hashset.mem
let size  : set -> int  = Hashset.size
let empty : set -> bool = Hashset.empty
let iter  : (int -> unit) -> set -> unit = Hashset.iter
let add   : set -> int -> unit = Hashset.add
let remove : set -> int -> unit = Hashset.remove
let minus : set -> set -> set = Hashset.minus
let cup : set -> set -> set = Hashset.cup
let cap : set -> set -> set = Hashset.cap
let choose : set -> int = Hashset.choose
let from_list : int list -> set = Hashset.from_list

let print_charset (s : set) = 
  if size s > 126 then
    let negset = minus (create_full ()) s in
      Printf.printf "neg {";
      Hashset.iter (fun c-> Printf.printf "%s," (char_as_string c)) negset;
      Printf.printf "}"
  else
    (Printf.printf "{";
     Hashset.iter (fun c-> Printf.printf "%s," (char_as_string c)) s;
     Printf.printf "}")


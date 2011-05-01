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

(** 
  Hashsets as a parameterized type using Hashtbl.
*)
type 'e hashset = ('e, unit) Hashtbl.t

(** Set "construction" - really just an alias for Hashtbl.create.
    @param n Desired size
    @return A new (empty) set
*)
let create (n : int) : 'e hashset =
  Hashtbl.create n

(** Set copying.
    @param s Set to copy
    @return A mostly-shallow copy of [s]
*)
let copy (s : 'e hashset) : 'e hashset =
  Hashtbl.copy s

(** Remove all elements.
    @param s Set to clear
*)
let clear (s : 'e hashset) : unit =
  Hashtbl.clear s

(** Set membership.
    @param s A set
    @param e An element
    @return [true] if [e] in [s]; false otherwise
*)
let mem (s : 'e hashset) (e : 'e) : bool = 
  Hashtbl.mem s e

(** Set size.
    @param s A set
    @return The number of elements in s
*)
let size  (s : 'e hashset) : int = 
  Hashtbl.length s

(** Test for emptiness.
    @param s A set
    @return [true] if [s] has no elements
*)
let empty (s : 'e hashset) : bool =
  size s = 0

(** Iterate.
    @param f Function to apply to each element of [s]
    @param s A set
*)
let iter (f : 'e -> unit)  (s : 'e hashset) : unit =
  Hashtbl.iter (fun x _ -> f x) s

exception Exists

(** @Return [true] if [s] has an element that satisfies predicate [f];
    false otherwise

*)
let exists (f : 'e -> bool) 
           (s : 'e hashset) : bool =
  try
    iter (fun x -> if f x then raise Exists) s;
    false
  with Exists -> true

(** Fold.
    @param f Function of type ['e -> 'a -> 'a], where ['e] is the element
    type of [s] and ['a] is the type of an accumulator
    @param s A set
    @param a An initial accumulator value
    @return [f e{_n} ( ... (f e{_1} a))]
*)
let fold (f : 'e -> 'a -> 'a) (s: 'e hashset) (a : 'a) =
  Hashtbl.fold (fun e _ acc -> f e acc) s a

(** Filter (to list).  
    @param f Function of type ['e -> bool], where ['e] is the element type
    of [s].
    @return A list of elements of [s] that satisfy [f e]
*)
let filter (f : 'e -> bool) (s : 'e hashset) : 'e list =
  fold (fun x acc -> if f x then x::acc else acc) s []

(** Split a set based on a predicate (to a pair of lists)
    @param f Function of type ['e -> bool], where ['e] is the element type
    @return A pair of lists [(s_yes, s_no)], where [s_yes] contains
            elements for which [f e == true], and [s_no] contains
            elements for which [f e == false].
*)
let split (f : 'e -> bool) (s : 'e hashset) : ('e list * 'e list) =
    fold (fun x (yes, no) -> if f x then (x::yes, no) else (yes, x::no)
	 ) s ([], [])

exception EmptySet

(** Get an element
    @return An arbirary element from the set; raises 
            [EmptySet] if no such element exists
*)
let choose (s: 'e hashset) : 'e =
  let thelist = filter (fun x -> true) s in
    try 
      List.hd thelist
    with _ -> raise EmptySet

(** In-place add.
    @param s A Set
    @param e Element to add to [s]
*)
let add (s : 'e hashset) (e : 'e) : unit = 
  Hashtbl.replace s e ()

(** Create a set of one element
    @param e Element to add
    @return A set containing just [e]
*)
let singleton (e : 'e) : 'e hashset =
  let res = create 1 in 
    add res e;
    res

(** Convert hashset to list
    @param s A set
    @return A list that contains all the elements of [s]
*)
let to_list (s : 'e hashset) : 'e list =
  filter (fun _ -> true) s

(** Convert list to hashset
    @param s A list
    @return A hashset that contains all the elements of [s]
*)
let from_list (s : 'e list) : 'e hashset =
  let res = create (List.length s) in
    List.iter (fun x -> add res x) s;
    res

(** In-place removal of an element. Ignored if element is not found.
    @param s A set
    @param e An element
*)
let remove (s : 'e hashset) (e : 'e) : unit = 
  Hashtbl.remove s e

(** Applicative set minus.
    @param lhs Set to remove from
    @param rhs Elements to remove
    @return [lhs \ rhs]
*)
let minus (lhs : 'e hashset) (rhs : 'e hashset) : 'e hashset =
    let newset = copy lhs in
      iter (remove newset) rhs;
      newset

(** Applicative set union.
    @param s1 A set
    @param s2 Another set
    @return [s1] union [s2]
*)
let cup  (s1 : 'e hashset) (s2 : 'e hashset) : 'e hashset = 
  let newset = copy s1 in
    iter (add newset) s2;
    newset

(** Applicative set intersection.
    @param s1 A set
    @param s2 Another set
    @return [s1] intersect [s2]
*)
let cap (s1 : 'e hashset) (s2 : 'e hashset) : 'e hashset = 
  let newset = create (min (size s1) (size s2)) in
  let adder otherset elt = if mem otherset elt then add newset elt in
    if size s1 > size s1 then
      iter (adder s1) s2
    else
      iter (adder s2) s1;
    newset

(** Set equality.
    @param s1 A set
    @param s2 Another set
    @return [true] iff s1 == s2
*)
let eq (s1 : 'e hashset) (s2 : 'e hashset) : bool =
  if size s1 == size s2 then
    (try 
       iter (fun x -> if not (mem s2 x) then raise Exists) s1;
       true
     with Exists ->
       false)
 else
   false

(** Filter duplicate elements from a list
    @param l List
    @return [l] with duplicate entries removed (last instance in [l] of
    any equivalence class is preserved)
*)
let unique (l : 'a list) : 'a list =
  let newset = create (List.length l) in
    List.iter (add newset) l;
    List.rev (fold (fun x acc -> x::acc) newset [])


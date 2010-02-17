open Format

module type S = sig
  type elt
  type t

  val unions : t list -> t
  val from_list : elt list -> t
  val to_list : t -> elt list
  val pretty : formatter -> (formatter -> elt -> unit) -> t -> unit
end

module Make (Set : Set.S) = struct
  
  type elt = Set.elt

  type t = Set.t

  let unions lst = List.fold_left Set.union Set.empty lst

  let from_list lst = 
    List.fold_left (fun set x -> Set.add x set) Set.empty lst

  let to_list set =
    Set.fold (fun e lst -> e :: lst) set []    

  let pretty formatter print_elt set =
    let is_first = ref true in
    let f elt =
      if !is_first then 
        (is_first := false; 
         print_elt formatter elt)
      else 
        (pp_print_string formatter ",";
         pp_print_space formatter ();
         print_elt formatter elt) in
      pp_open_box formatter 0;
      pp_print_string formatter "{";
      pp_print_space formatter ();
      pp_open_box formatter 0;
      Set.iter f set;
      pp_close_box formatter ();
      pp_print_space formatter ();
      pp_print_string formatter "}";
      pp_close_box formatter ()

end

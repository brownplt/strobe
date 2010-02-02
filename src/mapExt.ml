module type S = sig
  type key
  type +'a t

  val from_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list
  val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val join : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
end

module Make (Ord: Map.OrderedType) (Map : Map.S with type key = Ord.t) = struct

  type key = Ord.t

  type +'a t = 'a Map.t

  let from_list lst = 
    List.fold_left (fun m (k, v) -> Map.add k v m) Map.empty lst

  let to_list m = 
    Map.fold (fun k v lst -> (k, v) :: lst) m []

  let union f m1 m2 = 
    let rec g (k1, v1) (k2, v2) =
      if Ord.compare k1 k2 = 0 then (k1, f v1 v2)
      else raise Not_found
    in from_list (List.map2 g (to_list m1) (to_list m2))

  let join f m1 m2 =
    let mk k v acc = 
      if Map.mem k acc then 
        Map.add k (f v (Map.find k acc)) acc (* f m1-val  m2-val *)
      else 
        Map.add k v acc
    in Map.fold mk m1 m2 (* m2 is the accumulator *)
     

end

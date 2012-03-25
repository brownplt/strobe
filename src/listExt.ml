include List

let rec tails (lst : 'a list) : 'a list list = match lst with
  | [] -> [ [] ]
  | _ :: lst' -> lst :: (tails lst')

let rec pairs (lst1 : 'a list) (lst2 : 'b list) : ('a * 'b) list = 
  match lst1 with
    | [] -> []
    | (x :: rest) -> (map (fun y -> (x, y)) lst2) @ (pairs rest lst2)
    
let iter_pairs (f : 'a -> 'a -> unit) (lst : 'a list) : unit =
  let g lst = match lst with
    | x :: rest -> iter (f x) rest
    | _ -> () in
  iter g (tails lst)
    
let rec map2_noerr (f : 'a -> 'b -> 'c) (xs : 'a list) (ys : 'b list) =
  match (xs, ys) with
    | [], _ -> []
    | _, [] -> []
    | x :: xs', y :: ys' -> (f x y) :: map2_noerr f xs' ys'
      
let rec filter_map (f : 'a -> 'b option) (xs : 'a list) : 'b list =
  match xs with
    | [] -> []
    | x :: xs' -> match f x with
  | None -> filter_map f xs'
  | Some y -> y :: (filter_map f xs')

let split_at (n : int) (l : 'a list) : ('a list * 'a list) =
  let rec helper n (revhd, tl) = match n, tl with
    | 0, _ 
    | _, [] -> ((List.rev revhd), tl)
    | _, hd::tl' -> helper (n-1) (hd::revhd, tl')
  in helper n ([], l)

let remove_dups l =
  let rec helper l = match l with
    | [] -> []
    | [h] -> [h]
    | h::(m::t as tl) -> if h = m then (helper tl) else h::(helper tl)
  in helper (List.sort compare l)

let create n v = 
  let rec helper i acc =
    if i <= 0 then acc else helper (i-1) (v::acc)
  in helper n []

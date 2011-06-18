include List

let rec tails (lst : 'a list) : 'a list list = match lst with
  | [] -> [ [] ]
  | _ :: lst' -> lst :: (tails lst')
    
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


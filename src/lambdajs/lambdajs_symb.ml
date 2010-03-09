open Prelude
open Lambdajs_cps
open Lambdajs_cfa
open Lambdajs_syntax
open Lambdajs_lattice

module H = Hashtbl

type k = LB | UB

type bound =
  | BoundId of id * int * k
  | BoundLoc of Loc.t * int * k
  | BoundInt of int
  | BoundNegInf
  | BoundPosInf
  | BoundSum of bound * bound

type range = 
  | Bounded of bound * bound
  | Irrelevant


type constr = LEq of bound * bound

let constraints : constr list ref = ref []

let new_constraint constr =
  constraints := constr :: !constraints


let ranges : (int, range IdMap.t * range Heap.t) H.t = H.create 500

let set_id_range (x : id) (n : int) (b : range) = 
  let (env, heap) = H.find ranges n in
    begin match b with
      | Irrelevant -> ()
      | Bounded (lb, ub) ->
          new_constraint (LEq (lb, BoundId (x, n, LB)));
          new_constraint (LEq (BoundId (x, n, UB), ub))
    end;
    H.replace ranges n (IdMap.add x b env, heap)

let set_loc_range (x : Loc.t) (n : int) (b : range) = 
  let (env, heap) = H.find ranges n in
    begin match b with
      | Irrelevant -> ()
      | Bounded (lb, ub) ->
          new_constraint (LEq (lb, BoundLoc (x, n, LB)));
          new_constraint (LEq (BoundLoc (x, n, UB), ub))
    end;
    H.replace ranges n (env, Heap.add x b heap)
  

let flow (m : int) (n : int) : unit = 
  if H.mem ranges n then
    failwith "reflowing to a node"
  else
    H.replace ranges n (H.find ranges m)


let calc_val (n : int) (v : cpsval) = match v with
  | Id x -> 
      let (env, _) = H.find ranges n in
        IdMap.find x env
  | Const (Exprjs_syntax.CInt x) -> Bounded (BoundInt x, BoundInt x)
  | _ -> Irrelevant

let loc_at (x : id) (n : int) : Loc.t option = 
  match locations (Range.up (Type.up (H.find heaps n) 
                               (lookup x (H.find envs n)))) with
    | [loc] -> Some loc
    | _ -> None

let rec calc (m : int) (exp : cpsexp) = match exp with
  | Bind ((n, _), x, bindexp, cont) ->
      let n' = cpsexp_idx cont in
        flow n n';
        let x_range = match bindexp with
          | Object _ -> Irrelevant
          | UpdateField _ -> Irrelevant
          | Let v -> calc_val n v
          | Op1 (Deref, Id y) -> 
              begin match loc_at y n with
                | Some loc -> 
                    Bounded (BoundLoc (loc, n, LB), BoundLoc (loc, n, UB))
                | None -> Irrelevant
              end
          | Op1 (Ref, v) ->
              set_loc_range (Loc n) n' (calc_val n v);
              Irrelevant
          | Op2 (SetRef, Id x, v) ->
              begin match loc_at x n, calc_val n v with
                | Some loc, Bounded (lb, ub) ->
                    set_loc_range loc n (Bounded (lb, ub));
                    Irrelevant
                | _ -> Irrelevant
              end
          | Op2 (op2, v1, v2) ->
              begin match op2, calc_val n v1, calc_val n v2 with
                | Prim2 "+",
                  Bounded (v1_lb, v1_ub), Bounded (v2_lb, v2_ub) ->
                    Bounded (BoundSum (v1_lb, v2_lb),
                             BoundSum (v1_ub, v2_ub))
                | Prim2 "<=",
                    Bounded (v1_lb, v1_ub), Bounded (v2_lb, v2_ub) ->
                    Bounded (v1_lb, v2_ub)
                | Op2Infix JavaScript_syntax.OpAdd, 
                  Bounded (v1_lb, v1_ub), Bounded (v2_lb, v2_ub) ->
                    Bounded (BoundSum (v1_lb, v2_lb),
                             BoundSum (v1_ub, v2_ub))
                | Op2Infix JavaScript_syntax.OpLEq,
                    Bounded (v1_lb, v1_ub), Bounded (v2_lb, v2_ub) ->
                    Bounded (v1_lb, v2_ub)
                | _ -> Irrelevant
              end
        in 
          set_id_range x n' x_range;
          calc n cont
  | If ((n, _), Id x, true_cont, false_cont) ->
      let _ = calc_val n (Id x) in
      let n_true = cpsexp_idx true_cont in
      let n_false = cpsexp_idx false_cont in
        flow n n_true;
        flow n n_false;
        set_id_range x n_false 
          (Bounded (BoundId (x, n, LB), BoundId (x, n, UB)));
        calc n true_cont;
        calc n false_cont
  | Fix ((n, _), binds, cont) ->
      let calc_lambda (_, _, body) = 
        H.add ranges (cpsexp_idx body) (IdMap.empty, Heap.empty);
        calc (cpsexp_idx body) body in
        List.iter calc_lambda binds;
        flow n (cpsexp_idx cont);
        calc n cont
  | App ((n, _), _, _) ->
      let succs = try H.find call_graph n with Not_found -> IntSet.empty in
      let env, heap = H.find ranges n in
      let bound_id succ_n x _ = 
        new_constraint (LEq (BoundId (x, n, UB), BoundId (x, succ_n, UB)));
        new_constraint (LEq (BoundId (x, succ_n, LB), BoundId (x, n, LB))) in
      let bound_loc succ_n x _ = 
        new_constraint (LEq (BoundLoc (x, n, UB), BoundLoc (x, succ_n, UB)));
        new_constraint (LEq (BoundLoc (x, succ_n, LB), BoundLoc (x, n, LB))) in
      let per_succ succ_n =
        IdMap.iter (bound_id succ_n) env; 
        Heap.iter (bound_loc succ_n) heap;in
      IntSet.iter per_succ succs

  | _ -> ()


     

module Pretty = struct

  open Format
  open FormatExt

  let p_k k = match k with
    | LB -> text "L"
    | UB -> text "U"

  let rec p_bound bound = match bound with
    | BoundId (x, n, k) -> squish [ p_k k; text x; text "_"; int n ]
    | BoundLoc (l, n, k) -> squish [ p_k k; Loc.pp l; text "_"; int n ]
    | BoundInt n -> int n
    | BoundNegInf -> text "+inf"
    | BoundPosInf -> text "-inf"
    | BoundSum (b1, b2) -> sep [ p_bound b1; text "+"; p_bound b2 ]

  let p_constr c = match c with
    | LEq (b1, b2) -> horz [ p_bound b1; text "<="; p_bound b2 ]


  let p_id_range fmt n x range : unit = match range with
    | Irrelevant ->  ()
    | Bounded (lb, ub) ->
        horz [ p_bound lb; 
               text "<="; squish [ text x; text "_"; int n ]; text "<=";
               p_bound ub ] fmt;
        pp_print_newline fmt ()

  let p_loc_range fmt n l range : unit = match range with
    | Irrelevant -> ()
    | Bounded (lb, ub) ->
        horz [ p_bound lb; 
               text "<="; squish [ Loc.pp l; text "_"; int n ]; text "<=";
               p_bound ub ] fmt;
        pp_print_newline fmt ()

  let p_range_at_node (fmt : formatter) (n : int) (env, heap) : unit = 
    IdMap.iter (p_id_range fmt n) env;
    Heap.iter (p_loc_range fmt n) heap

  let p_ranges fmt = 
    H.iter (p_range_at_node fmt) ranges


end

let calc_bounds cpsexp = 
  H.add ranges (cpsexp_idx cpsexp) (IdMap.empty, Heap.empty);
  calc (cpsexp_idx cpsexp) cpsexp;
  Pretty.p_ranges Format.std_formatter;
  Format.print_newline ();
  Format.print_newline ();
  Format.print_newline ();

  List.iter
    (fun c -> 
       Pretty.p_constr c Format.std_formatter;
       Format.pp_print_newline Format.std_formatter ())
    !constraints
       

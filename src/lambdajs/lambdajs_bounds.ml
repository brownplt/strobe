open Prelude
open Lambdajs_cps
open Lambdajs_cfa
open Lambdajs_syntax
open Lambdajs_lattice

module Bound = struct

  type dir = L | U

  type t = 
    | Id of id * int * dir
    | Loc of Loc.t * int * dir
    | Int of int
    | NegInf
    | PosInf
    | Sum of t * t

  open FormatExt

  let p_dir d = match d with
    | U -> text "u"
    | L -> text "l"

  let rec pp t = match t with
    | Id (x, n, d) -> squish [ p_dir d; text x; text "_"; int n ]
    | Loc (l, n, d) -> squish [ p_dir d; Loc.pp l; text "_"; int n ]
    | Int n -> int n
    | NegInf -> text "-inf.0"
    | PosInf -> text "+inf.0"
    | Sum (t1, t2) -> horz [ pp t1; text "+"; pp t2 ]

end

module 

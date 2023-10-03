(*
Assign3-3:
HX-2023-09-26: 10 points
//
The function [list_nchoose(xs)(n0)]
returns all the subsequences of xs that are
of length n0.
//
let
list_nchoose
(xs: 'a list)(n0: int): 'a list list =
//
Please give a NON-RECURSIVE implementation of
list_nchoose based on list-combinators. Note that
the order of the elements in a list representation
of a subsequenc is SIGNIFICANT. For instance, [1;2]
and [2;1] are DIFFERENT.
//
*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let list_nchoose xs n0 =
  let rec extend prefix remaining n =
    if n = 0 then
      list_make_fwork (fun work -> work prefix)
    else
      match remaining with
      | [] -> list_make_fwork (fun _ -> ())
      | x::xs' -> 
        let with_x = extend (x::prefix) xs' (n-1) in
        let without_x = extend prefix xs' n in
        list_concat [with_x; without_x]
  in
  extend [] xs n0
;;

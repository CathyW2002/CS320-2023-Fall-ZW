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
  let len = list_length xs in
  
  let indices_to_subset indices =
    list_make_fwork (fun work ->
      list_foreach indices (fun i -> work (List.nth xs i))
    )
  in

  let init_indices = list_make_fwork (fun work -> int1_foreach len (fun i -> work [i])) in
  
  let extend_subsets_indices current_indices =
    list_make_fwork (fun work ->
      list_foreach current_indices (fun subset ->
        let last_idx = List.hd subset in
        int1_foreach len (fun i ->
          if i > last_idx && not (List.mem i subset) then 
            work (i :: subset)
        )
      )
    )
  in

  let rec build_combinations k current_indices =
    if k = n0 then 
      list_make_fwork (fun work ->
        list_foreach current_indices (fun subset -> work (indices_to_subset subset))
      )
    else 
      build_combinations (k + 1) (extend_subsets_indices current_indices)
  in
  
  build_combinations 1 init_indices
;;

let list_length xs = 
  list_foldleft xs 0 (fun acc _ -> acc + 1)
;;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let next_level current_sum =
  let rec generate_pairs i j =
    if i + j = current_sum then
      [(i, j)] @ (if i > 0 then generate_pairs (i - 1) (j + 1) else [])
    else
      []
  in
  generate_pairs current_sum 0  

let theNatPairs: unit -> (int * int) strcon =
  let rec stream_pairs current_sum rest_of_pairs () =
    match rest_of_pairs with
    | [] -> 
      let next_sum = current_sum + 1 in
      let next_pairs = next_level next_sum in
      stream_pairs next_sum next_pairs ()
    | pair :: remaining_pairs -> 
      StrCons (pair, stream_pairs current_sum remaining_pairs)
  in
  stream_pairs 0 [(0, 0)]

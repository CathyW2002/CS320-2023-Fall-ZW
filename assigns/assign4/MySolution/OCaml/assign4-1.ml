#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a stream = Cons of 'a * (unit -> 'a stream);;

let rec ln2_terms i sign = 
  Cons (sign *. 1. /. float_of_int i, fun () -> ln2_terms (i + 1) (sign *. -1.))
;;

let rec partial_sums sum stream = 
  match stream with
  | Cons (value, next) -> 
      let new_sum = sum +. value in
      Cons (new_sum, fun () -> partial_sums new_sum (next ()))
;;

let the_ln2_stream () =
  let terms = ln2_terms 1 1. in
  partial_sums 0. terms
;;

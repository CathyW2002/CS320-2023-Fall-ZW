#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec ln2_terms i sign () = 
  StrCons (sign /. float_of_int i, ln2_terms (i + 1) (-. sign))
;;

let rec partial_sums acc stream_gen () = 
  match stream_gen () with
  | StrNil -> StrNil  (* No more elements *)
  | StrCons (value, next_gen) -> 
    let new_sum = acc +. value in
    StrCons (new_sum, partial_sums new_sum next_gen) 
;;

let the_ln2_stream: unit -> float strcon = 
  partial_sums 0.0 (ln2_terms 1 1.0)
;;


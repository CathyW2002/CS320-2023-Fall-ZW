#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a stream_cell = Nil | Cons of 'a * 'a stream
and 'a stream = unit -> 'a stream_cell;;

(* Function to generate the ln(2) series stream. *)
let the_ln2_stream: float stream =
  let rec next_term coeff index sign = 
    let current_value = sign *. (1.0 /. coeff) in
    let next_stream () = next_term (coeff +. 1.0) (index + 1) (sign *. -1.0) in (* Prepare the next term *)
    Cons (current_value, next_stream)  (* Construct the current stream cell *)
  in
  let rec partial_sum_stream value current_stream = 
    match current_stream () with
    | Nil -> Nil  (* No more elements *)
    | Cons (head, tail) -> 
        let new_sum = value +. head in  (* Add the current term to the partial sum *)
        let next_stream () = partial_sum_stream new_sum tail in  (* Prepare the next stream *)
        Cons (new_sum, next_stream)  (* Construct the current stream cell with the new sum *)
  in
  partial_sum_stream 0.0 (next_term 1.0 1 1.0)  (* Initialize the streams with the first term *)
;;

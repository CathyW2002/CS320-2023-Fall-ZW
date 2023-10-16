#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a strcon = Cons of 'a * (unit -> 'a strcon);;

let the_ln2_stream () =
  let rec next i =
    let value = if i mod 2 = 0 then -.1.0 /. float_of_int i else 1.0 /. float_of_int i in
    Cons (value, fun () -> next (i + 1))  (* Construct the stream element *)
  in
  next 1  (* Start with the first element *);;

(* Function to generate the partial sums of the series *)
let rec partial_sums n stream =
  let rec helper count acc (Cons (head, tail)) =
    if count <= 0 then acc
    else helper (count - 1) (acc +. head) (tail ())
  in
  helper n 0.0 stream;;

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(* Define the 'strcon' type once, and do not redefine it in this session. *)
type 'a strcon = Cons of 'a * (unit -> 'a strcon)
;;

(* Helper function to generate the next element of the ln(2) series. *)
let rec next_ln2_element i sum =
  let new_sum = if i mod 2 = 0 then sum -. 1.0 /. float_of_int i else sum +. 1.0 /. float_of_int i in
  Cons(new_sum, fun () -> next_ln2_element (i + 1) new_sum)
;;

(* The stream representing the partial sums of the ln(2) series. *)
let the_ln2_stream () : float strcon = next_ln2_element 1 1.0 ;;

(* Usage: function to retrieve the n-th element of a stream *)
let rec nth_stream_element (Cons (head, tail)) n =
  if n = 1 then head
  else nth_stream_element (tail ()) (n - 1)
;;

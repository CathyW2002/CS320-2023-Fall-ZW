#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(* Definitions of the custom stream types and exceptions *)
exception Empty;;

type 'a strcon = unit -> 'a strcell;;
and 'a strcell = StrNil | StrCons of 'a * 'a strcon;;

(* Implementing the stream for the ln(2) series *)
let the_ln2_stream : float strcon =
  let rec next i sum =
    let new_sum = if i mod 2 = 0 then sum -. (1. /. float_of_int i) else sum +. (1. /. float_of_int i) in
    fun () -> StrCons(new_sum, next (i + 1) new_sum)
  in
  next 1 1.0;;

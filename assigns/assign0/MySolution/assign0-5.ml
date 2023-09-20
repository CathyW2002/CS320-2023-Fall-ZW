(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
*)

let stringrev (cs: string): string =
  let len = String.length cs in
  String.init len (fun i -> cs.[len - 1 - i])
;;

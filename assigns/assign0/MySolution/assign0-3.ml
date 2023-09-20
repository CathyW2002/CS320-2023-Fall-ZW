(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

let int2str (i0: int): string =
  if i0 = 0 then "0" else
  let rec loop n acc =
      if n = 0 then acc
      else loop (n / 10) (Char.escaped (Char.chr (48 + (n mod 10))) ^ acc)
  in
  if i0 < 0 then "-" ^ loop (-i0) "" else loop i0 ""
;;

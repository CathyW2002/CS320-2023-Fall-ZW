(*
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
*)

let ord = Char.code

let str2int(cs: string): int =
  let len = String.length cs in

  (* Convert a character to its integer representation *)
  let char_to_digit c = ord c - ord '0' in

  let rec aux idx acc =
    if idx = len then acc
    else
      let digit = char_to_digit (string_get cs idx) in
      aux (idx + 1) (acc * 10 + digit)
  in

  if string_get cs 0 = '-' then
    -1 * (aux 1 0)  (* If the string starts with '-', it's a negative number *)
  else
    aux 0 0
;;

(*
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
*)

let str2int (cs: string): int =
  let len = String.length cs in
  let base = 10 in
  let rec helper idx acc =
    if idx = len then acc
    else
      let char_val = Char.code cs.[idx] - Char.code '0' in
      helper (idx + 1) (acc * base + char_val)
  in
  helper 0 0
;;

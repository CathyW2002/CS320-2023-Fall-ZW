(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0

let int2str(i0: int): string =
  (* Check if the number is negative *)
  let is_negative = i0 < 0 in
  
  (* Work with the absolute value of the number *)
  let num = if is_negative then (-i0) else i0 in

  (* Convert a single digit to its character representation *)
  let digit_to_char n = chr (ord '0' + n) in

  (* Recursively extract digits and convert them to characters *)
  let rec extract_digits n acc =
    if n = 0 && acc = [] then [digit_to_char 0] (* handle case when i0 = 0 *)
    else if n = 0 then acc
    else extract_digits (n / 10) ((digit_to_char (n mod 10)) :: acc)
  in

  (* Construct the string from the list of character digits *)
  let digits = extract_digits num [] in
  let result = String.concat "" (List.map str digits) in

  (* Add negative sign if the original number was negative *)
  if is_negative then "-" ^ result else result
;;

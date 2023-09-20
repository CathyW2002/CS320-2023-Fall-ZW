(*
assign1-4: 20 points
Given two strings ds1 and ds2 of digits, please
implement intrep_add(ds1)(ds2) that returns the
string representing the sum of the two integers
represented by ds1 and ds2. Note that the returned
string should contain no leading zeros. (And we
use the empty string to represent the number zero).

fun
intrep_add(ds1: string)(ds2: string): string

For instance, intrep_add("1116123")("222987") = "1339110"

Note that ds1 and ds2 can be arbitrarily long. Thus,
converting ds1 and ds2 to integers can cause overflow.
*)
let intrep_add (ds1: string) (ds2: string): string =
  let carry = ref 0 in
  let len1 = String.length ds1 in
  let len2 = String.length ds2 in
  let max_len = max len1 len2 in
  let result = ref "" in

  for i = 0 to max_len - 1 do
      let d1 = if i < len1 then int_of_char ds1.[len1 - 1 - i] - int_of_char '0' else 0 in
      let d2 = if i < len2 then int_of_char ds2.[len2 - 1 - i] - int_of_char '0' else 0 in
      let sum = d1 + d2 + !carry in
      result := (Char.escaped (char_of_int ((sum mod 10) + int_of_char '0'))) ^ !result;
      carry := sum / 10;
  done;

  (* If there's still a carry left, prepend it to the result *)
  if !carry > 0 then result := (string_of_int !carry) ^ !result;

  (* Remove leading zeros *)
  let rec remove_leading_zeros s =
      if s = "" then "0" else if s.[0] = '0' then remove_leading_zeros (String.sub s 1 ((String.length s) - 1)) else s
  in
  remove_leading_zeros !result
;;

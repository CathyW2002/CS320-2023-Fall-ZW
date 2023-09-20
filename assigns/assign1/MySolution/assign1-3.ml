(*
assign1-3: 10 points
A 3-letter sequence abc is 132-like
if a < c < b holds. For instance, 123 is
not 132-like; but 596 is 132-like.

A string is 132-avoid if there is no subsequence
abc in this string that is 132-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 132-avoid;
For instance, 987654321 is 132-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 165 is 132-like.

Please implement a function string_avoid_132 that
checks if a given string is 132-avoid; the function
returns true if and only if the given string is 132-
avoid.

fun string_avoid_132(cs: string): bool
*)

let string_avoid_132 cs =
  let n = String.length cs in
  
  (* Helper function to check if characters at positions i, j, k form a 132-like sequence *)
  let is_132 i j k =
    let a = int_of_char cs.[i] in
    let b = int_of_char cs.[j] in
    let c = int_of_char cs.[k] in
    a < c && c < b
  in
  
  let rec loop i j k =
    if i >= n || j >= n || k >= n then
      true
    else if is_132 i j k then
      false
    else
      loop i (j + 1) (k + 1) || loop i j (k + 1) || loop (i + 1) j k
  in

  loop 0 1 2
;;

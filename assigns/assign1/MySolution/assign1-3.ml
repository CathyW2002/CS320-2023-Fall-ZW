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

let string_length = String.length;;

let string_avoid_132 (cs:string): bool =
  let n = string_length cs in
  
  for i = 0 to n-3 do
    for j = i+1 to n-2 do
      if cs.[j] > cs.[i] then
        for k = j+1 to n-1 do
          if cs.[k] < cs.[j] && cs.[k] > cs.[i] then
            (* Found a 132 pattern *)
            false
        done;
    done;
  done;
  true
;;

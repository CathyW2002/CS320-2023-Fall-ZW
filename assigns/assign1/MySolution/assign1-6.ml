(*
assign1-6: 20 bonus points
A 4-letter sequence abcd is 1324-like
if a < c < b < d holds. For instance, 1234 is
not 132-like; but 2547 is 1324-like.

A string is 1324-avoid if there is no subsequence
abc in this string that is 1324-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 1324-avoid;
For instance, 987654321 is 1324-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 1657 is 1324-like.

Please implement a function string_avoid_1324 that
checks if a given string is 1324-avoid; the function
returns true if and only if the given string is 1324-
avoid.

fun string_avoid_1324(cs: string): bool
*)

(* ****** ****** *)

let string_avoid_1324 (cs: string): bool =
  let n = String.length cs in
  try
    for i = 0 to n-4 do
      for j = i+1 to n-3 do
        for k = j+1 to n-2 do
          for l = k+1 to n-1 do
            if cs.[i] < cs.[k] && cs.[k] < cs.[j] && cs.[j] < cs.[l] then
              (* Found a 1324 pattern *)
              raise Exit;
          done;
        done;
      done;
    done;
    true (* If loops complete without finding 1324 pattern *)
  with Exit -> false
;;

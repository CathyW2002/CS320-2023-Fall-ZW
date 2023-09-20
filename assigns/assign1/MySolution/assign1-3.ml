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
  let stack = Stack.create () in
  let c = ref (int_of_char '9' + 1) in
  
  for i = (n - 1) downto 0 do
    if (int_of_char cs.[i]) < !c then
      (* Found a 132 pattern *)
      exit 1
    else
      (* Check and update c *)
      while (not (Stack.is_empty stack)) && (int_of_char cs.[i] > Stack.top stack) do
        c := Stack.pop stack
      done;

      (* Add current element to stack *)
      Stack.push (int_of_char cs.[i]) stack
  done;
  true
;;

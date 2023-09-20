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

let string_avoid_132 (cs: string): bool =
  (* Convert string to list of integers for easier manipulation *)
  let nums = 
    cs 
    |> String.to_seq 
    |> Seq.map (fun ch -> int_of_char ch - int_of_char '0') 
    |> List.of_seq 
  in

  (* Initialize an empty stack and a potential "c" value *)
  let stack = Stack.create () in
  let ref_c = ref (-1) in

  let is_132_avoided = ref true in

  (* Check each number in the list *)
  List.iter (fun num ->
    (* If current number is smaller than potential "c" value,
       then there's a 132-like sequence *)
    if num < !ref_c then
      is_132_avoided := false
    else begin
      (* While the stack isn't empty and the current number is 
         smaller than the top of the stack *)
      while not (Stack.is_empty stack) && num < Stack.top stack do
        ref_c := Stack.pop stack
      done;
      Stack.push num stack
    end
  ) nums;

  !is_132_avoided
;;
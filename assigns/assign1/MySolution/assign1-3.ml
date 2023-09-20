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
    let n = String.length cs in
    let exists_smaller_than_a_after_b a b =
        for k = b+1 to n-1 do
            if cs.[k] > cs.[a] && cs.[k] < cs.[b] then
                return false
        done;
        true
    in
    try
        for i = 0 to n-3 do
            for j = i+1 to n-2 do
                if cs.[i] < cs.[j] then
                    if not (exists_smaller_than_a_after_b i j) then
                        raise Exit
                end
            done
        done;
        true
    with Exit -> false
;;

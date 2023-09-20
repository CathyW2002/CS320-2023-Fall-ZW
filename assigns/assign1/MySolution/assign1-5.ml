(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)

#use "./../assign1.ml";;
#use "./../../../classlib/OCaml/MyOCaml.ml";;

let string_longest_ascend (xs:string):string =
  let n = String.length xs in
  if n = 0 then "" else
  let max_start = ref 0 and max_end = ref 0 in
  let start = ref 0 and end_ = ref 0 in

  while !end_ < n - 1 do
    if xs.[!end_] <= xs.[!end_ + 1] then
      incr end_
    else begin
      if !end_ - !start > !max_end - !max_start then begin
        max_start := !start;
        max_end := !end_;
      end;
      start := !end_ + 1;
      end_ := !start;
    end
  done;

  (* Handle the scenario where the longest sequence is at the end of the string *)
  if !end_ - !start > !max_end - !max_start then begin
    max_start := !start;
    max_end := !end_;
  end;

  String.sub xs !max_start (!max_end - !max_start + 1)
;;



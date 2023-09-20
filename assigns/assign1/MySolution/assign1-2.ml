(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)

let list_reverse lst =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (x::acc) xs
  in aux [] lst
;;

let list_make_fwork (fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref [] in
  let work x0 = res := x0 :: !res in
  fwork work;
  list_reverse !res
;;

let string_make_fwork fwork =
  let xs = Array.of_list(list_make_fwork fwork) in
  String.init (Array.length xs) (fun i -> xs.(i))
;;

let string_merge cs1 cs2 =
  let index1 = ref 0 and index2 = ref 0 in
  let length1 = String.length cs1 and length2 = String.length cs2 in

  string_make_fwork (fun yield ->
    while !index1 < length1 && !index2 < length2 do
      if cs1.[!index1] <= cs2.[!index2] then (
        yield cs1.[!index1];
        incr index1
      ) else (
        yield cs2.[!index2];
        incr index2
      )
    done;

    while !index1 < length1 do
      yield cs1.[!index1];
      incr index1
    done;

    while !index2 < length2 do
      yield cs2.[!index2];
      incr index2
    done
  )
;;


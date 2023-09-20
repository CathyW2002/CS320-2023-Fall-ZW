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

let string_make_fwork(fwork: (char -> unit) -> unit): string =
  let xs =
    Array.of_list(list_make_fwork(fwork)) 
  in String.init (Array.length(xs)) (fun i -> xs.(i))

let string_of_char c = String.make 1 c

let string_to_fwork s f = String.iter f s

let string_merge cs1 cs2 =
  let fwork_merge f = 
    string_to_fwork cs1 f;
    string_to_fwork cs2 f 
  in
  string_make_fwork fwork_merge
;;

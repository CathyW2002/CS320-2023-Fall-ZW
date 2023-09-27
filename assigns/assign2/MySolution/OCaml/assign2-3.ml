#use "./../../../classlib/OCaml/MyOcaml.ml";; 
#use "./../../assign2.ml";;


type ('xs, 'x0, 'r0) foldleft = 'xs -> 'r0 -> ('r0 -> int -> 'x0 -> 'r0) -> 'r0
type ('xs, 'x0) iforeach = 'xs -> (int -> 'x0 -> unit) -> unit

let list_foldleft: ('a list, 'b, int) foldleft =
  fun xs r0 fopr ->
    let rec aux xs i r0 =
      match xs with
      | [] -> r0
      | x :: xs' -> aux xs' (i + 1) (fopr r0 i x)
    in
    aux xs 0 r0
;;

let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun xs f ->
    let folder r0 i x = f i x; i + 1 in
    let _ = foldleft xs 0 folder in
    ()
;;

let list_iforeach =
  fun xs -> foldleft_to_iforeach(list_foldleft)(xs)
;;
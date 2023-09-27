#use "./../../../classlib/OCaml/MyOCaml.ml"

type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist

(* Define an exception for subscripting *)
exception MySubscript;;

(* Define functions for forward and reverse traversal of mylist *)
let rec
mylist_foreach
(xs: 'a mylist)
(work: 'a -> unit): unit =
match xs with
| MyNil -> ()
| MyCons(x1, xs) ->
  (work(x1); mylist_foreach(xs)(work))
| MySnoc(xs, x1) ->
  (mylist_foreach(xs)(work); work(x1))
| MyReverse(xs) -> mylist_rforeach(xs)(work)
| MyAppend2(xs1, xs2) ->
  (mylist_foreach(xs1)(work); mylist_foreach(xs2)(work))

and
mylist_rforeach
(xs: 'a mylist)
(work: 'a -> unit): unit =
match xs with
| MyNil -> ()
| MyCons(x1, xs) ->
  (mylist_rforeach(xs)(work); work(x1))
| MySnoc(xs, x1) ->
  (work(x1); mylist_rforeach(xs)(work))
| MyReverse(xs) -> mylist_foreach(xs)(work)
| MyAppend2(xs1, xs2) ->
  (mylist_rforeach(xs2)(work); mylist_rforeach(xs1)(work))
;;

(* Define a function to calculate the length of mylist *)
let rec mylist_length (xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons(_, xs) -> 1 + mylist_length xs
  | MySnoc(xs, _) -> 1 + mylist_length xs
  | MyReverse(xs) -> mylist_length xs
  | MyAppend2(xs1, xs2) -> mylist_length xs1 + mylist_length xs2
;;

(* Define a function to convert mylist to a standard OCaml list *)
let rec to_std_list (xs: 'a mylist): 'a list =
  match xs with
  | MyNil -> []
  | MyCons(head, tail) -> head :: to_std_list tail
  | MySnoc(tail, head) -> to_std_list tail @ [head]
  | MyReverse(xs) -> List.rev (to_std_list xs)
  | MyAppend2(xs1, xs2) -> to_std_list xs1 @ to_std_list xs2
;;

(* Define a function to handle subscript exceptions *)
let
mylist_subscript_exn
( (*void*) ): 'a = raise MySubscript;;

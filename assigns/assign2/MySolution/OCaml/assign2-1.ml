#use "./../../../classlib/OCaml/MyOCaml.ml"

let
mylist_subscript_exn
( (*void*) ): 'a = raise MySubscript;;

let rec
mylist_length (xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons(_, xs) -> 1 + mylist_length xs
  | MySnoc(xs, _) -> 1 + mylist_length xs
  | MyReverse(xs) -> mylist_length xs
  | MyAppend2(xs1, xs2) -> mylist_length xs1 + mylist_length xs2
;;

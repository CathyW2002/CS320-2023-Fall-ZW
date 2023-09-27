#use "./../../../classlib/OCaml/MyOCaml.ml"
(* Define exception *)
exception MySubscript;;

(* Define mylist data type *)
type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist

(* Helper function to handle MyReverse case *)
let rec reverse xs =
  match xs with
  | MyNil -> MyNil
  | MyCons (x, xs) -> MySnoc (reverse xs, x)
  | MySnoc (xs, x) -> MyCons (x, reverse xs)
  | MyReverse xs -> xs
  | MyAppend2 (xs1, xs2) -> MyAppend2 (reverse xs2, reverse xs1)

(* mylist_get_at implementation *)
let rec mylist_get_at (xs: 'a mylist) (i0: int): 'a =
  if i0 < 0 then raise MySubscript
  else
    match xs with
    | MyNil -> raise MySubscript
    | MyCons (x, xs) ->
      if i0 = 0 then x
      else mylist_get_at xs (i0 - 1)
    | MySnoc (xs, x) -> 
      let len = mylist_length xs in
      if i0 = len then x
      else mylist_get_at xs i0
    | MyReverse xs -> mylist_get_at (reverse xs) i0
    | MyAppend2 (xs1, xs2) ->
      let len1 = mylist_length xs1 in
      if i0 < len1 then mylist_get_at xs1 i0
      else mylist_get_at xs2 (i0 - len1)
;;

(* Define mylist_length as a placeholder; replace with your actual implementation *)
let rec mylist_length (xs: 'a mylist): int = 
  match xs with
  | MyNil -> 0
  | MyCons(_, xs) -> 1 + mylist_length xs
  | MySnoc(xs, _) -> 1 + mylist_length xs
  | MyReverse(xs) -> mylist_length xs
  | MyAppend2(xs1, xs2) -> mylist_length xs1 + mylist_length xs2
;;

(*
Assign3-1:
HX-2023-09-26: 10 points
A matrix can be represented as a list of lists.
For instance, [[1;2;3];[4;5;6];[7;8;9]] represents
the following 3x3 square matrix:
1 2 3
4 5 6
7 8 9
Please implement matrix_transpose that returns
the transpose of a given matrix:
let rec
matrix_transpose(xss: 'a list list): 'a list list
For instance, the transpose of the above matrix
is given as follows:
1 4 7
2 5 8
3 6 9
You are allowed to define recursive functions to
solve this problem.
*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec first_elements xs = 
  list_make_fwork (fun work -> 
    list_foreach xs (fun x -> match x with 
      | hd :: tl -> work hd 
      | [] -> ())
  )
;;

let rec remove_first_elements xs = 
  list_make_fwork (fun work -> 
    list_foreach xs (fun x -> match x with 
      | hd :: tl -> work tl 
      | [] -> ())
  )
;;

let rec matrix_transpose xss = 
  if list_forall xss (fun x -> x = []) 
  then [] 
  else first_elements xss :: matrix_transpose (remove_first_elements xss)
;;

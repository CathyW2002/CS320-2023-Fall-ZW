#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a strcon =
  | StrNil
  | StrCons of 'a * (unit -> 'a strcon)

type 'a stream = unit -> 'a strcon

let rec insert_everywhere x lst =
  let rec aux prev acc = function
    | [] -> list_revapp acc [[x]]
    | (hd :: tl) as l -> 
      let new_list = list_revapp prev (x :: l) in
      aux (hd :: prev) (new_list :: acc) tl
  in
  aux [] [] lst

let rec permutations lst =
  match lst with
  | [] -> [[]]
  | hd :: tl ->
    let perm_tl = permutations tl in
    let rec aux acc = function
      | [] -> acc
      | l :: ls ->
        let new_lists = insert_everywhere hd l in
        aux (list_revapp new_lists acc) ls
    in
    aux [] perm_tl

let rec list_to_stream lst () =
  match lst with
  | [] -> StrNil
  | hd :: tl -> StrCons (hd, list_to_stream tl)

let list_permute (xs: 'a list) : 'a list stream =
  let perms = permutations xs in
  list_to_stream perms

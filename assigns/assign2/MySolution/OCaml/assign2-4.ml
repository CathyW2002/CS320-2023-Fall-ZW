#use "./../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign2.ml";;
let string_sepjoin_list (sep: string)(xs: string list): string =
  let combine acc s = 
    if acc = "" then s
    else acc ^ sep ^ s
  in
  List.fold_left combine "" xs
;;

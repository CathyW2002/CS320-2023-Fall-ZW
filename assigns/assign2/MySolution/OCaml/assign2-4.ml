#use "./../../../classlib/OCaml/MyOCaml.ml";; 
#use "./../../assign2.ml";;

let string_sepjoin_list (sep: string)(xs: string list): string =
  let add_sep x = if x = "" then [] else [x; sep] in
  let with_sep = List.flatten (List.map add_sep xs) in
  let result = string_concat_list with_sep in
  let result_length = String.length result in
  let sep_length = String.length sep in
  if result_length >= sep_length then
    String.sub result 0 (result_length - sep_length)
  else
    result
;;

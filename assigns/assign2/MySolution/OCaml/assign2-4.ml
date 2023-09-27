#use "./../../../classlib/OCaml/MyOcaml.ml";; 
#use "./../../assign2.ml";;

let string_sepjoin_list (sep: string)(xs: string list): string =
  let with_sep = List.map (fun x -> [x; sep]) xs in
  let flattened = list_concat with_sep in
  let result = string_concat_list flattened in
  let result_length = String.length result in
  let sep_length = String.length sep in
  if result_length >= sep_length then
    String.sub result 0 (result_length - sep_length)
  else
    result
;;
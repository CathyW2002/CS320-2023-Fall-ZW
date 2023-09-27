#use "./../../../classlib/OCaml/MyOcaml.ml";; 
#use "./../../assign2.ml";;

let list_make_fwork f = 
  let acc = ref [] in
  f (fun x -> acc := x :: !acc);
  List.rev !acc
;;

let list_foreach xs f = List.iter f xs;;

let string_make_fwork f =
  let buffer = Buffer.create 16 in
  f (fun c -> Buffer.add_char buffer c);
  Buffer.contents buffer
;;

let string_foreach s f = String.iter f s;;

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

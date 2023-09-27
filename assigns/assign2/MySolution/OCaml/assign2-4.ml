#use "./../../../classlib/OCaml/MyOcaml.ml";; 
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

let string_concat_list(css: string list): string =
  string_make_fwork(
    fun work -> list_foreach css (fun cs -> string_foreach cs work)
  )
;;

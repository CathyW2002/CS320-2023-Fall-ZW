(*
Assign3-4:
HX-2023-09-26: 20 points
Given a word x of length n, another word is a buddy
of x if x and y differ exactly at one position. For
instance, "live" is a buddy "love" (and "love" is also
a buddy of "live").
//
Please give a NON-RECURSIVE implementation of
list_of_buddies that returns a list of all the buddies
of a given word.
//
let
list_of_buddies(word: string): string list = ...

(*
FYI. The concept of word buddies is used in the following game:
https://xanadu-lang.github.io/xats2js/docgen/CodeBook/Doublet/2020-11-29/
https://github.com/xanadu-lang/xats2js/tree/master/docgen/CodeBook/Doublet
*)
*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let list_of_buddies(word: string): string list =
  let len = string_length word in
  let all_chars = "abcdefghijklmnopqrstuvwxyz" in
  
  let buddies_for_position i =
    let original_char = string_get_at word i in
    let replaced = 
      list_make_fwork (fun work ->
        string_foreach all_chars (fun ch ->
          if ch <> original_char then
            let new_word = 
              string_tabulate len (fun j -> if j = i then ch else string_get_at word j) 
            in
            work new_word
        )
      )
    in
    replaced
  in

  list_make_fwork (fun work ->
    int1_foreach len (fun i -> list_foreach (buddies_for_position i) work)
  )
;;

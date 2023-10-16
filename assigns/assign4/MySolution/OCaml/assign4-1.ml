#use "./../../../../classlib/OCaml/MyOCaml.ml";;

exception Empty

type 'a stream = unit -> 'a strcell
and 'a strcell = StrNil | StrCons of 'a * 'a stream;;

let the_ln2_stream : float stream =
  let rec next i =
    fun () ->
      let value = if i mod 2 = 0 then -(1. /. float_of_int i) else 1. /. float_of_int i in
      StrCons (value, next (i + 1))
  in
  next 1;;

let partial_sums (s : float stream) : float stream =
  let rec helper acc s =
    match s () with
    | StrNil -> fun () -> StrNil
    | StrCons (value, rest) ->
      let new_acc = acc +. value in
      fun () -> StrCons (new_acc, helper new_acc rest)
  in
  helper 0.0 s;;

let the_ln2_partial_sums = partial_sums the_ln2_stream;;

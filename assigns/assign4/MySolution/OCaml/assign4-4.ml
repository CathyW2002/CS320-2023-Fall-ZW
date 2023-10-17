#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a stream = StrNil | StrCons of 'a * (unit -> 'a stream);;

let list_permute (xs : 'a list) : 'a list stream =
  let rec extract_at idx lst =
    let rec extract acc i = function
      | [] -> None  
      | h :: t as l -> if i = idx then Some (h, list_revapp acc t) else extract (h :: acc) (i + 1) l
    in
    extract [] 0 lst
  in

  let rec interleave x lst =
    let rec aux pre suf acc = match suf with
      | [] -> (List_reverse (x :: pre)) :: acc
      | hd::tl ->
        let new_pre = list_revapp tl (x :: pre) in
        aux (hd::pre) tl (new_pre :: acc)
    in
    aux [] lst []
  in

  let rec permute lst =
    match lst with
    | [] -> StrCons ([], fun () -> StrNil)
    | _  ->
      let n = foreach_to_length(list_foreach)(lst) in
      let rec loop i =
        if i >= n then StrNil
        else
          match extract_at i lst with
          | None -> StrNil 
          | Some (element, rest) ->
            let subperms_stream = permute rest in
            let rec distribute perms_stream =
              match perms_stream with
              | StrNil -> loop (i + 1)
              | StrCons (perm, get_next_stream) ->
                let interleaved = interleave element perm in
                let rec concat_interleaved il =
                  match il with
                  | [] -> distribute (get_next_stream ())
                  | h :: t -> StrCons (h, fun () -> concat_interleaved t)
                in
                concat_interleaved interleaved
            in
            distribute subperms_stream
      in loop 0
  in
  permute xs
;;

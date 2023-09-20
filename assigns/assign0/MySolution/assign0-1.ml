(*
Assign0-1: 10 points
Please find the first integer N such that the
evaluation of fact(N) in OCaml yields an Overflow
exception.
*)

let rec fact (x: int): int =
  if x = 0 then x else fact(x+1);;


let find_overflow () =
  let rec aux n =
    let value = 
      try
        Some (fact n)
      with
        | _ -> None
    in
    match value with
    | Some v when v < 0 -> n 
    | _ -> aux (n+1)
  in aux 1;;


let myan = find_overflow () 
;;

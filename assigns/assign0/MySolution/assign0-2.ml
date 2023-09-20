(*
Assign0-2: 10 points
Please implement a function that tests whether a
given natural number is a prime:
fun isPrime(n0: int): bool
*)

let isPrime (n0: int): bool =
  if n0 <= 1 then false
  else if n0 <= 3 then true
  else if n0 mod 2 = 0 || n0 mod 3 = 0 then false
  else
    let rec check i =
      if i * i > n0 then true
      else if n0 mod i = 0 || n0 mod (i + 2) = 0 then false
      else check (i + 6)
    in check 5
;;

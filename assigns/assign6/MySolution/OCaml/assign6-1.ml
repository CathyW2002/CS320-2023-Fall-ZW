#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let custom_map f xs =
  foreach_to_map_list list_foreach xs f
;;

let string_concat_list_with_space (css: string list): string =
  let rec aux acc = function
  | [] -> acc
  | [x] -> acc ^ x
  | x :: xs -> aux (acc ^ x ^ " ") xs
  in aux "" css
;;

let rec sexpr_to_string e =
  match e with
  | SInt n -> string_of_int n
  | SAdd exprs -> "(add " ^ (string_concat_list_with_space (custom_map sexpr_to_string exprs)) ^ ")"
  | SMul exprs -> "(mul " ^ (string_concat_list_with_space (custom_map sexpr_to_string exprs)) ^ ")"
;;

let digit_to_int d = digit_of_char d

let parse_number : int parser =
  let* digits = many1 digit in  (* many1 ensures we have at least one digit *)
  let number = list_foldleft digits 0 (fun acc d -> acc * 10 + digit_to_int d) in
  pure number
;;

let parse_space = satisfy char_iswhitespace ;;

let parse_spaces : unit parser = many parse_space >| ()
;;

let parse_symbol sym : unit parser =
  let* _ = literal sym in
  let* _ = parse_spaces in
  pure ()
;;

let rec parse_expr xs =
  match parse_number xs with
  | Some (number, rest) -> Some (SInt number, rest)
  | None -> 
      match parse_add xs with
      | Some _ as add_result -> add_result
      | None -> 
          match parse_mul xs with
          | Some _ as mul_result -> mul_result
          | None -> parse_parenthesized parse_expr xs

and parse_exprs xs =
  many1' (fun () -> parse_spaces >> parse_expr) xs

and parse_add xs =
  bind (parse_symbol "add") (fun _ ->
  bind parse_exprs (fun exprs -> pure (SAdd exprs))) xs

and parse_mul xs =
  bind (parse_symbol "mul") (fun _ ->
  bind parse_exprs (fun exprs -> pure (SMul exprs))) xs
;;

let char c : char parser =
  satisfy (fun x -> x = c)

let parse_parenthesized p : 'a parser =
  let* _ = parse_spaces in  (* Allow spaces before the opening parenthesis *)
  let* _ = char '(' in
  let* result = p in
  let* _ = char ')' in
  let* _ = parse_spaces in  (* Allow spaces after the closing parenthesis *)
  pure result
;;

let sexpr_parse s =
  let chars = string_listize s in
  match parse_parenthesized parse_expr chars with
  | Some (expr, remaining) -> 
      if remaining = [] then Some expr
      else None (* Additional characters remain unparsed *)
  | None -> None
;;
//

(*
Example (Accepted Strings):
sexpr_parse "(add 1 2 3)" = Some (SAdd [SInt 1; SInt 2; Int 3])
sexpr_parse "(mul (add 1 2) 3 (mul 1))" = Some (SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1]])
//
Example (Rejected Strings):
sexpr_parse "()" = None
sexpr_parse "(add)" = None
sexpr_parse "(add 1 2))" = None
sexpr_parse "((mul 1 2)" = None
//
*)

(* ****** ****** *)


Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)

(* ****** ****** *)

(* end of [CS320-2023-Fall-assigns-assign6.ml] *)

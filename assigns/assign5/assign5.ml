#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)
(* Token types *)
type token =
  | T_Int of int
  | T_Add
  | T_Mul
  | T_OpenParen
  | T_CloseParen
;;

(* Tokenize the input string *)
let tokenize (s : string) : token list =
  let rec aux chars acc =
    match trim chars with
    | [] -> list_reverse acc
    | '(' :: rest -> aux rest (T_OpenParen :: acc)
    | ')' :: rest -> aux rest (T_CloseParen :: acc)
    | 'a' :: 'd' :: 'd' :: rest -> aux rest (T_Add :: acc)
    | 'm' :: 'u' :: 'l' :: rest -> aux rest (T_Mul :: acc)
    | c :: rest when c >= '0' && c <= '9' ->
        let num, remaining = extract_number (c :: rest) in
        aux remaining (T_Int num :: acc)
    | _ -> failwith "Invalid token"
  and extract_number chars =
    (* Extract a number and return the remaining characters *)
    let rec aux acc chars =
      match chars with
      | c :: rest when c >= '0' && c <= '9' ->
          aux (acc * 10 + digit_of_char c) rest
      | _ -> acc, chars
    in aux 0 chars
  in aux (string_listize s) []
;;


(* Parse the tokens into an expression *)
let rec parse_tokens tokens =
  match tokens with
  | T_OpenParen :: T_Add :: rest -> parse_expr_list T_Add rest
  | T_OpenParen :: T_Mul :: rest -> parse_expr_list T_Mul rest
  | T_Int n :: rest -> Some (Int n, rest)
  | _ -> None

and parse_expr_list op tokens =
  let rec aux acc tokens =
    match tokens with
    | T_CloseParen :: rest -> 
        if acc = [] then None (* Ensure there's at least one expression *)
        else Some ((match op with
                    | T_Add -> Add (list_reverse acc)
                    | T_Mul -> Mul (list_reverse acc)), rest)
    | _ -> (match parse_tokens tokens with
            | Some (expr, rest) -> aux (expr :: acc) rest
            | None -> None)
  in aux [] tokens
;;

let parse (s : string) : expr option = (* YOUR CODE *)
  match parse_tokens (tokenize s) with
  | Some (expr, []) -> Some expr
  | _ -> None
;;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a stream = unit -> 'a strcon

let rec gtree_streamize_dfs (tree: 'a gtree): 'a stream =
  let rec dfs t continuation = match t with
    | GTnil -> continuation ()
    | GTcons (value, subtrees) ->
        StrCons (value, fun () -> dfs_list subtrees continuation)
  and dfs_list trees continuation = match trees with
    | [] -> continuation ()
    | t::ts -> dfs t (fun () -> dfs_list ts continuation)
  in
  fun () -> dfs tree (fun () -> StrNil) 

let gtree_streamize_bfs (tree: 'a gtree): 'a stream =
  let bfs trees =
    let rec extract_values tree_list queue continuation = match tree_list with
      | [] -> (match queue with
          | [] -> continuation ()
          | _ -> extract_values queue [] continuation)
      | GTnil :: ts -> extract_values ts queue continuation
      | GTcons (value, subtrees) :: ts ->
          StrCons (value, fun () -> extract_values ts (queue @ subtrees) continuation)
    in
    extract_values trees [] (fun () -> StrNil)
  in
  fun () -> bfs [tree]

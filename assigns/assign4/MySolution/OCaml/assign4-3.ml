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
    let rec extract_values tree_list queue = match tree_list with
      | [] -> (match queue with
          | [] -> StrNil  
          | _ -> 
              let next_level = list_reverse queue in
              extract_values next_level [])
      | GTnil :: ts -> extract_values ts queue
      | GTcons (value, subtrees) :: ts ->
          let new_queue = list_revapp (list_reverse subtrees) queue in
          StrCons (value, fun () -> extract_values ts new_queue)
    in
    extract_values trees []
  in
  fun () -> bfs [tree]

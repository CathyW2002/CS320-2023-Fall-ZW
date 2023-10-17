#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a stream = unit -> 'a strcon;;

let gtree_streamize_dfs (tree: 'a gtree): 'a stream =
  let rec dfs t = match t with
    | GTnil -> StrNil
    | GTcons (value, subtrees) -> 
        StrCons (value, fun () -> dfs_list subtrees)
  and dfs_list trees = match trees with
    | [] -> StrNil
    | t::ts -> 
        let rec append_stream s1 s2 = match s1 with
          | StrNil -> s2 ()
          | StrCons (value, next_stream) -> StrCons (value, fun () -> append_stream (next_stream ()) s2)
        in
        append_stream (dfs t) (fun () -> dfs_list ts)
  in
  dfs tree
;;

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
;;

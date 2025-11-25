type 'a graph = Graph of ('a -> 'a list)

let depthSearch (Graph succ) start =
  let rec dfs visited node =
    if List.mem node visited then visited
    else List.fold_left dfs (node::visited) (succ node)
  in List.rev (dfs [] start);;
let g = Graph
(function
0 -> [3]
| 1 -> [0;2;4]
| 2 -> [1]
| 3 -> []
| 4 -> [0;2]
| n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
);;

let ds = depthSearch g 4;;

List.map print_int ds;; 

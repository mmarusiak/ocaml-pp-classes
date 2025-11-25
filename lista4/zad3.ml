type 'a bt = Empty | Node of 'a * 'a bt * 'a bt

let breadthBT tree =
  let rec bfs queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty::t -> bfs t acc
    | Node(v, l, r)::t -> bfs (t @ [l; r]) (v::acc)
  in bfs [tree] []

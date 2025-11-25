 type 'a bt = Empty | Node of 'a * 'a bt * 'a bt


 let rec internal_length tree depth =
  match tree with
  | Empty -> 0
  | Node(_, l, r) -> depth + internal_length l (depth+1) + internal_length r (depth+1)


let rec external_length tree depth =
  match tree with
  | Empty -> depth
  | Node(_, l, r) -> external_length l (depth+1) + external_length r (depth+1)

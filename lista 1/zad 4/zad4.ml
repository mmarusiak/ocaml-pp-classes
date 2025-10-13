let rec sqrtList(xs : int list) : int list
  = if List.tl(xs) = [] then [List.hd(xs) * List.hd(xs)] else List.hd(xs) * List.hd(xs) :: sqrtList(List.tl(xs));;

let () =
  let out = sqrtList [1; 2; 3; 4; 5] in
  let s = String.concat " " (List.map string_of_int out) in
  print_endline s
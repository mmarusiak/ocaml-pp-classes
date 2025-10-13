let rec flatten1 xs = if List.tl(xs) = [] then List.hd(xs) else List.hd(xs) @ flatten1(List.tl(xs));;

let () =
  let out = flatten1 [[1;2]; [3;4]; [5;6]] in
  let s = String.concat " " (List.map string_of_int out) in
  print_endline s
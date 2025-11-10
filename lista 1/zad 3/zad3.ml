let rec replicate (x, n) = 
  if n <= 0 then [] 
  else x :: replicate(x, n - 1);;

let () =
  let out = replicate ('a', 5) in
  let s = String.concat " " (List.map (String.make 1) out) in
  print_endline s
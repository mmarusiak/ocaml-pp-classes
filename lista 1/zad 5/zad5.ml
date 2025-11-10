let rec palindrome xs =
  (*List.rev xs = xs*)
  let rec last_and_rest ys =
    if List.tl ys = [] then
      (List.hd ys, [])
    else
      let (l, r) = last_and_rest (List.tl ys) in
      (l, (List.hd ys) :: r)
  in
  if xs = [] then true
  else if List.tl xs = [] then true
  else
    let (l, r) = last_and_rest xs in
    if l = List.hd xs then
      if r = [] then true
      else palindrome (List.tl r)
    else false

let () =
  let l = [1; 2; 1; 2; 2; 1; 1; 2; 1] in
  print_endline (string_of_bool (palindrome l))

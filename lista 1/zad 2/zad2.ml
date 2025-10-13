let rec count (xs, x) = if List.tl(xs) = [] then if List.hd(xs) = x then 1 else 0 else if List.hd(xs) = x then 1 + count(List.tl(xs), x) else count(List.tl(xs), x);;

let result = count ([1;2;3;4;5;1;1;1], 1);;
print_int result;;
let selectionSort asc x =
  let rec selectionSort result list =
    let rec getMax m l outList =
      match l with 
      | h::t -> 
        if (asc h m) then getMax h t (m::outList)
      else getMax m t (h::outList)
      | [] -> m, outList
    in 
    match list with
    | h::t -> let (max, l) = getMax h t [] in 
      selectionSort (max::result) l
    | [] -> result
  in selectionSort [] x;;


let rec insert cmp x lst =
  match lst with 
  | [] -> [x]
  | h::t -> if cmp x h then x :: h :: t
  else h :: insert cmp x t;;

let insertionSort cmp lst = 
  List.fold_left (fun acc x -> insert cmp x acc) [] lst

let list = [2; 3; 1; 21; -20];;
let sortedList = selectionSort (fun a b -> a > b) list

let rec split lst =
  match lst with
  | [] | [_] -> (lst, [])
  | a::b::t ->
      let (l1,l2) = split t in
      (a::l1, b::l2)
;;

let rec merge cmp l1 l2 =
  match (l1, l2) with
  | ([], ys) -> ys
  | (xs, []) -> xs
  | (x::xs', y::ys') ->
      (* Aby zachować stabilność: jeśli x nie jest "większe" niż y, 
         czyli !(cmp y x), to wybieramy x (lewy element) *)
      if cmp y x then y :: merge cmp l1 ys' else x :: merge cmp xs' l2
;;

let rec mergesort cmp lst =
  match lst with
  | [] | [_] -> lst
  | _ ->
      let (l1,l2) = split lst in
      let s1 = mergesort cmp l1 in
      let s2 = mergesort cmp l2 in
      merge cmp s1 s2
;;


let rec print_int_list lst =
  match lst with
  | [] -> print_string "[]"
  | [x] -> Printf.printf "[%d]" x
  | x :: xs ->
      Printf.printf "[%d; " x;
      let rec aux = function
        | [] -> print_string "]"
        | [y] -> Printf.printf "%d]" y
        | y :: ys ->
            Printf.printf "%d; " y;
            aux ys
      in
      aux xs
;;

print_int_list sortedList

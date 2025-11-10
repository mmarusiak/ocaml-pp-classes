let rec qsort xs =
  match xs with
  | [] -> []
  | x::rest ->
      let small = List.filter (fun y -> y < x) rest
      and large = List.filter (fun y -> y >= x) rest in
      qsort small @ (x :: qsort large)
;;


(*
Zamiast List.hd xs > x
Zamiast xs > rest
Teraz pivot nie wraca do large
Nie gubiemy elementów równych pivotowi (>= x obejmuje je)
*)

let rec qsort = function
  | [] -> []
  | x::xs ->
      let small = List.filter (fun y -> y < x) xs
      and large = List.filter (fun y -> y >= x) xs in
      qsort small @ (x :: qsort large)
;;

(*(fun y -> y > x) > (fun y -> y >= x)
*)
let curry3 f x y z = f (x,y,z);;
let uncurry3 f (x,y,z) = f x y z;;


let b_l_curry3 = fun f -> fun x -> fun y -> fun z -> f (x,y,z);;
let b_l_uncurry3 = fun f -> fun (x,y,z) -> f x y z;;

type 'a list = Nil | Cons of 'a * 'a list

let print n =
  let rec print' n =
    match n with Nil -> "]" | Cons (x, xs) -> x ^ ";" ^ print' xs
  in
  print_endline ("[" ^ print' n)

let empty = Nil

let cons x xs = Cons (x, xs)

let ( @@ ) x xs = cons x xs

let rec fold xs e_z e_s =
  match xs with Nil -> e_z | Cons (x, xs) -> e_s x (fold xs e_z e_s)

let length xs = fold xs 0 (fun _ xs -> xs + 1)

let append xs ys = fold xs ys cons

let map f xs = fold xs empty (fun x xs -> f x @@ xs)

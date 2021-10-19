type num = Z | S of num

let print n =
  let rec print' n = match n with Z -> "Z" | S x -> "S (" ^ print' x ^ ")" in
  print_endline (print' n)

let succ x = S x

let rec iter n e_z e_s = match n with Z -> e_z | S x -> e_s (iter x e_z e_s)

let plus n m = iter n m succ

let rec toNum n = match n with 0 -> Z | n -> succ (toNum (n - 1))

let toInt n = iter n 0 (( + ) 1)

let rec times n m = iter n Z (plus m)

let one = toNum 1

let two = toNum 2

let three = toNum 3

let rec pow n m = iter m one (times n)

let comp f g x = f (g x)

let repeat f n = iter n f (comp f)

let ack m = iter m succ (fun x n -> repeat x n one)

let t x y = x
let f x y = y
let branch e e1 e2 = e e1 e2

let pair e1 e2 k = k e1 e2
let fst e = e t
let snd e = e f

let l e f g = f e
let r e f g = g e
let case e e1 e2 = e e1 e2

let z n s = n
let s e n s = s (e n s)
let iter n ez es = n ez es

let plus n m = iter n m (fun x -> s x)
let times n m = iter n z (fun x -> plus m x)
let pow n m = iter m s z (fun x -> times n x)
let toInt e = iter e 0 (fun x -> x + 1)

let nil n c = n
let cons e1 e2 n c = c e1 (e2 n c)
let fold e en ec = e en ec

let length e = fold e 0 (fun n c -> c + 1)
let append xs ys = fold xs ys (fun n c -> cons n c)
let map f xs = fold xs nil (fun n c -> cons (f n) c)
let print e = fold e "$" (fun n c -> n ^ ":" ^ c)
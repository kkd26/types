include List

let t = "9" @@ "1" @@ "2" @@ "3" @@ empty

let tt = "5" @@ "3" @@ "7" @@ empty

let ttt = append t tt

let ttt = map (fun x -> x ^ x) ttt

let _ = print ttt

let a = length ttt

let _ = Format.printf "%d\n" a

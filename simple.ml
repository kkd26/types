type type_exp = Unit | Fun of type_exp * type_exp | Zero | Int | Bool

type exp =
  | Int of int
  | Bool of bool
  | Var of int
  | Unit
  | If of exp * exp * exp
  | Lambda of type_exp * exp
  | App of exp * exp
  | Abort of exp

let isValue = function
  | Unit -> true
  | Int _ -> true
  | Bool _ -> true
  | Lambda _ -> true
  | Var _ -> true
  | _ -> false

let rec shift i c = function
  | Var n -> if n >= c then Var (n + i) else Var n
  | Int n -> Int n
  | Bool b -> Bool b
  | If (e, e1, e2) -> If (shift i c e, shift i c e1, shift i c e2)
  | Unit -> Unit
  | Lambda (t, e) -> Lambda (t, shift i (c + 1) e)
  | App (e1, e2) -> App (shift i c e1, shift i c e2)
  | Abort e -> Abort (shift i c e)

let rec subst e n = function
  | Var m -> if n = m then e else Var m
  | Int n -> Int n
  | Bool b -> Bool b
  | If (e1, e2, e3) -> If (subst e n e1, subst e n e2, subst e n e3)
  | Unit -> Unit
  | Lambda (t, e1) -> Lambda (t, subst (shift 1 0 e) (n + 1) e1)
  | App (e1, e2) -> App (subst e n e1, subst e n e2)
  | Abort e1 -> Abort (subst e n e1)

let rec reduce = function
  | Var _ -> None
  | Int n -> None
  | Bool b -> None
  | If (e, e1, e2) -> (
      match e with
      | Bool b -> if b then Some e1 else Some e2
      | _ -> (
          match reduce e with Some e -> Some (If (e, e1, e2)) | None -> None))
  | Unit -> None
  | Lambda _ -> None
  | App (e1, e2) -> (
      match e1 with
      | Lambda (t, e) -> (
          if isValue e2 then Some (shift (-1) 0 (subst (shift 1 0 e2) 0 e))
          else
            match reduce e2 with Some e -> Some (App (e1, e)) | None -> None)
      | _ -> (
          match reduce e1 with Some e -> Some (App (e, e2)) | None -> None))
  | Abort e -> None

let doStep e = match reduce e with Some e -> e | None -> e

let rec reduceAll e = match reduce e with Some e -> reduceAll e | None -> e

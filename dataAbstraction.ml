module type BOOL = sig
  type t

  val yes : t
  val no : t
  val choose : t -> 'a -> 'a -> 'a
end

module M1 : BOOL = struct
  type t = int

  let yes = 1
  let no = 0
  let choose b e1 e2 = if b = 1 then e1 else e2
end

module M2 : BOOL = struct
  type t = unit option

  let yes = Some ()
  let no = None
  let choose b e1 e2 = match b with Some () -> e1 | None -> e2
end

module M3 : BOOL = struct
  type t = {f: 'a. 'a -> 'a -> 'a}

  let yes = {f= (fun a _ -> a)}
  let no = {f= (fun _ b -> b)}
  let choose b e1 e2 = b.f e1 e2
end

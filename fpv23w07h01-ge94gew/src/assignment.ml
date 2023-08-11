let f1 acc v = acc + 1

let f2 acc v = if List.length acc > List.length v then acc else v

let f3 acc (a, b) = acc @ [(b, a)]

let f4 acc v = v :: List.rev(acc)

let f5 acc (b, c) = fun a -> if (a = b) then c else acc a

let f6 acc v = match acc with
  | [] -> failwith "nicht definiert"
  | h :: _ -> (v h) :: acc

let f7 acc v = acc * acc * v
open Type

let rec add a b =
  match a with
  | Zero -> b
  | Succ n -> add n (Succ b)

let rec mul a b =
  match a with
  | Zero -> Zero
  | Succ n -> add b (mul n b)

let rec pow a b =
  match b with
  | Zero -> Succ Zero
  | Succ n -> mul a (pow a n)

let rec leq a b =
  match (a, b) with
  | (Zero, _) -> true
  | (Succ _, Zero) -> false
  | (Succ x, Succ y) -> leq x y
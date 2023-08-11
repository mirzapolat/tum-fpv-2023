open Type

let int_to_nat i =
  if i < 0 then failwith "negative arg"
  else
  let rec aux i acc =
    if i <= 0 then acc
    else aux (i - 1) (Succ acc)
  in
  aux i Zero

let nat_to_int n =
  if n = Zero then 0
  else
  let rec backw n acc =
    match n with
    | Succ n -> backw n (acc + 1)
    | Zero -> acc
  in
  backw n 0

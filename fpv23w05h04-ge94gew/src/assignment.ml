let rec eval_poly x poly =
  match poly with
  | [] -> 0.0
  | p :: oly -> (p *. (x ** float_of_int (List.length oly))) +. eval_poly x oly

let derive_poly poly = []
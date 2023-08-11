type unary_op = Neg
type binary_op = Add | Sub | Mul | Div
type rat = int * int (* num, denom *)
type expr = Const of rat
          | UnOp of unary_op * expr
          | BinOp of binary_op * expr * expr


let reduct (r : rat) =
  let (num, denom) = r in
  let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)
  in
  let g = gcd num denom
  in
  (num / g, denom / g)


let eval_expr _ = failwith "TODO: implement eval_expr"

(* sample inputs: should evaluate to (-3, 2) and (5, 1) respectively, after simplifying *)
let a67_ex1 = BinOp (Mul, BinOp (Sub, Const (3, 5), Const (2, 1)), BinOp (Div, Const (3, 2), Const (7, 5)))
let a67_ex2 = BinOp (Add, UnOp (Neg, a67_ex1), BinOp (Div, Const (7, 1), Const (2, 1)))

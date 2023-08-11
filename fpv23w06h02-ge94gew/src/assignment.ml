type unary_op = Neg
type binary_op = Add | Sub | Mul | Div
type rat = int * int (* num, denom *)
type expr = Const of rat
          | UnOp of unary_op * expr
          | BinOp of binary_op * expr * expr


let reduct r =
  let (num, denom) = r in
  let rec gcd a b =
    if b = 0 then a
    else gcd b (a mod b) in
  let g = gcd num denom in
  (num / g, denom / g)

let rec eval_expr e =
  match e with
  | Const r -> reduct r
  | UnOp (Neg, e) ->
    let (num, denom) = eval_expr e in
    (-num, denom)
  | BinOp (op, e1, e2) ->
    let (n1, d1) = eval_expr e1 in
    let (n2, d2) = eval_expr e2 in
    match op with
      | Add -> reduct (n1 * d2 + n2 * d1, d1 * d2)
      | Sub -> reduct (n1 * d2 - n2 * d1, d1 * d2)
      | Mul -> reduct (n1 * n2, d1 * d2)
      | Div -> reduct (n1 * d2, d1 * n2) (* Kehrwert *)


(* sample inputs: should evaluate to (-3, 2) and (5, 1) respectively, after simplifying *)
let a67_ex1 = BinOp (Mul, BinOp (Sub, Const (3, 5), Const (2, 1)), BinOp (Div, Const (3, 2), Const (7, 5)))
let a67_ex2 = BinOp (Add, UnOp (Neg, a67_ex1), BinOp (Div, Const (7, 1), Const (2, 1)))

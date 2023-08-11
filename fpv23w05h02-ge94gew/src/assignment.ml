let rec interleave2 one two =
  match (one, two) with _, [] -> one
  | [], _ -> two
  | x::xs, y::ys -> x::y::interleave2 xs ys

let rec interleave3 l1 l2 l3 =
  match (l1, l2, l3) with
  | _, _, [] -> interleave2 l1 l2
  | _, [], _ -> interleave2 l1 l3
  | [], _, _ -> interleave2 l2 l3
  | x::xs, y::ys, z::zs -> x::y::z::interleave3 xs ys zs

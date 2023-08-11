let foo x y b =
  let rec rotation x y b =
    if x > y then
      let t = x in
      let x = y in
      let y = t in
      rotation x y b
    else if x < y then
      if b then rotation (x+1) y (not b)
      else rotation x (y-1) (not b)
    else x
  in rotation x y b
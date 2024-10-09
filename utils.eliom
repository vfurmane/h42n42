let%shared tl_of_center (x, y) r = x -. r, y -. r
let%shared px_of_float f = Format.sprintf "%dpx" (int_of_float f)

let%shared random_float_in_range ~min ~max =
  let f = Random.float (max -. min) in
  f +. min

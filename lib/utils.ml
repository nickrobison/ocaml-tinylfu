let lg x = log x /. log 2.
let int_ceil x = int_of_float (ceil x)

let next_power_of_two x =
  let m = int_ceil (lg (float_of_int x)) in
  1 lsl m

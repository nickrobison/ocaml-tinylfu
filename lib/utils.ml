let lg x = log x /. log 2.
let int_ceil x = int_of_float (ceil x)
let next_power_of_two x = int_ceil (lg (float_of_int x))

type t = { table : int array; sample_size : int; block_mask : int; size : int }

let make size =
  let table_size = max (Utils.next_power_of_two size) 8 in
  let table = Array.make table_size 0 in
  let sample_size = if size == 0 then 10 else size * 10 in
  let block_mask = (Array.length table lsr 3) - 1 in
  { table; sample_size; block_mask; size }

let sample_size t = t.sample_size
let block_mask t = t.block_mask
let table t = t.table
let size t = t.size

let increment_at t i j =
  let offset = j lsl 2 in
  let mask = 0xf lsl 2 in
  if t.table.(i) land mask != mask then (
    t.table.(i) <- t.table.(i) + (1 lsl offset);
    true)
  else false

let reset _t = ()
let block t entry = (entry land t.block_mask) lsl 3

let increment t ~entry =
  let index = Array.make 8 0 in
  let block = block t entry in
  for i = 0 to 3 do
    let h = entry lsr i lsl 3 in
    index.(i) <- (h lsr 1) land 15;
    let offset = h land 1 in
    index.(i + 4) <- block + offset + (i lsl 1)
  done;
  let incr = increment_at t in
  let added =
    incr index.(4) index.(0)
    || incr index.(5) index.(1)
    || incr index.(6) index.(2)
    || incr index.(7) index.(3)
  in
  let size' = +t.size in
  if added && size' == t.sample_size then reset t else ();
  { t with size = size' }

let frequency t ~entry =
  let count = Array.make 4 0 in
  let block = block t entry in
  for i = 0 to 3 do
    let h = entry lsr i lsl 3 in
    let idx = (h lsr 1) land 15 in
    let offset = h land 1 in
    let tv = t.table.(block + offset + (i lsl 1)) in
    count.(i) <- (tv lsr idx lsl 2) land 0xf
  done;
  print_endline "";
  print_int count.(0);
  print_int count.(1);
  print_int count.(2);
  print_int count.(3);
  print_endline "";
  print_endline ";";
  max (max count.(0) count.(1)) (max count.(2) count.(3))

open Tiny_lfu

let test_make () =
  let size = 214748365 in
  let sketch = Sketch.make size in
  let table_length = Array.length (Sketch.table sketch) in
  Alcotest.(check int)
    "Should have correct table length"
    (Utils.next_power_of_two size)
    table_length;
  Alcotest.(check int)
    "Should have correct sample size" 2147483650
    (Sketch.sample_size sketch);
  Alcotest.(check int)
    "Should have correct block mask"
    ((table_length lsr 3) - 1)
    (Sketch.block_mask sketch)

let test_increment () =
  let sketch =
    Sketch.make 16 |> Sketch.increment ~entry:1 |> Sketch.increment ~entry:1
  in
  let pp_arry = Fmt.array Fmt.int in
  Fmt.(pf stdout "%a" pp_arry (Sketch.table sketch));
  Alcotest.(check int)
    "Should have estimate for 1" 2
    (Sketch.frequency sketch ~entry:1);
  Alcotest.(check int)
    "Should have have estimate for 0" 0
    (Sketch.frequency sketch ~entry:0)

let test_increment_max () =
  let entry = 42 in
  let sketch = Sketch.make 512 in
  let c = Array.make 20 0 in
  let s' = Array.fold_left (fun s _ -> Sketch.increment s ~entry) sketch c in
  Alcotest.(check int)
    "Should have max count value" 15
    (Sketch.frequency s' ~entry)

let v =
  let open Alcotest in
  ( "Sketch tests",
    [
      test_case "Test Creation" `Quick test_make;
      test_case "Test Increment" `Quick test_increment;
      test_case "Test Max Increment" `Quick test_increment_max;
    ] )

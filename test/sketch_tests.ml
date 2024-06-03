open Tiny_lfu

let make () =
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

let increment () =
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

let increment_max () =
  let entry = 42 in
  let sketch = Sketch.make 512 in
  let c = Test_utils.int_seq 0 20 () in
  let s' = Seq.fold_left (fun s _ -> Sketch.increment s ~entry) sketch c in
  Alcotest.(check int)
    "Should have max count value" 15
    (Sketch.frequency s' ~entry)

let increment_zero () =
  let sketch = Sketch.make 64 |> Sketch.increment ~entry:0 in
  Alcotest.(check int)
    "Should have correct estimate" 1
    (Sketch.frequency sketch ~entry:0)

let reset () =
  let sketch = Sketch.make 64 in
  let reset = ref false in
  let c = Test_utils.int_seq 0 (20 * 64) () in
  let c' = Seq.take_while (fun _ -> !reset != true) c in
  let s' =
    Seq.fold_left
      (fun s idx ->
        let ss = Sketch.increment s ~entry:idx in
        if Sketch.size ss != idx then reset := true else ();
        ss)
      sketch c'
  in
  Alcotest.(check bool) "Should have reset" true !reset;
  Alcotest.check Test_utils.at_most "Should be less than half the size"
    (Sketch.sample_size s' / 2)
    (Sketch.size s')

let heavy_hitters () =
  let sketch = Sketch.make 512 in
  let seqs = Test_utils.int_seq 100 100000 () in
  let s' =
    Seq.fold_left (fun s entry -> Sketch.increment s ~entry) sketch seqs
  in
  let double_seq = Test_utils.int_seq 0 10 ~step:2 () in
  let s2 =
    Seq.fold_left
      (fun s i ->
        let j = Test_utils.int_seq 0 i () in
        Seq.fold_left (fun ss entry -> Sketch.increment ss ~entry) s j)
      s' double_seq
  in
  (* A perfect popularity count yields an array [0, 0, 2, 0, 4, 0, 6, 0, 8, 0] *)
  let popularity = Array.make 10 0 in
  Array.mapi_inplace (fun entry _ -> Sketch.frequency s2 ~entry) popularity;
  Array.iteri
    (fun idx v ->
      let open Test_utils in
      match idx with
      | 0 | 1 | 3 | 5 | 7 | 9 -> Alcotest.check at_most "" popularity.(2) v
      | 2 -> Alcotest.check at_most "" popularity.(4) popularity.(2)
      | 4 -> Alcotest.check at_most "" popularity.(6) popularity.(4)
      | 6 -> Alcotest.check at_most "" popularity.(8) popularity.(6)
      | _ -> ())
    popularity

let v =
  let open Alcotest in
  ( "Sketch tests",
    [
      test_case "Test Creation" `Quick make;
      test_case "Test Increment" `Quick increment;
      test_case "Test Max Increment" `Quick increment_max;
      test_case "Test Incrementing 0" `Quick increment_zero;
      test_case "Test reset" `Quick reset;
      test_case "Test Heavy Hitters" `Quick heavy_hitters;
    ] )

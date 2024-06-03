open Tiny_lfu

let test_power_of_2 () =
  Alcotest.(check int)
    "Should have correct power of 2 value" 268435456
    (Utils.next_power_of_two 214748365);
  Alcotest.(check int)
    "Should have correct power of 2 value" 134217728
    (Utils.next_power_of_two 128849018)

let v =
  let open Alcotest in
  ("Util tests", [ test_case "Test Next Power of 2" `Quick test_power_of_2 ])

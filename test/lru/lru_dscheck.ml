module I = struct
  type t = int

  let weight _ = 1
  let pp = Fmt.int
  let equal (a : int) b = a = b
  let hash (i : int) = Hashtbl.hash i
end

module C = Lru.Make (I) (I)

let put_get () =
  Atomic.trace (fun () ->
      let cache = C.make 32 in

      let total_entries = 4 in

      Atomic.spawn (fun () ->
          for i = 1 to total_entries do
            C.put cache i i
          done);

      let fetched = ref 0 in
      Atomic.spawn (fun () ->
          for i = 1 to total_entries do
            match C.get cache i with
            | None -> ()
            | Some v ->
                assert (v == !fetched + 1);
                fetched := !fetched + 1
          done);

      Atomic.final (fun () ->
          Atomic.check (fun () -> !fetched == total_entries)))

let put_remove () =
  Atomic.trace (fun () ->
      let cache = C.make 32 in

      let total_entries = 4 in

      Atomic.spawn (fun () ->
          for i = 1 to total_entries do
            C.put cache i i
          done);

      Atomic.spawn (fun () ->
          for i = 1 to total_entries do
            C.remove cache i
          done);

      Atomic.final (fun () -> Atomic.check (fun () -> C.is_empty cache)))

let two_puts () =
  Atomic.trace (fun () ->
      let cache = C.make 32 in
      let total_entries = 4 in

      for i = 1 to 2 do
        Atomic.spawn (fun () ->
            for j = 1 to total_entries do
              C.put cache j i
            done)
      done;

      Atomic.final (fun () ->
          Atomic.check (fun () -> total_entries = C.size cache)))

let two_domains () =
  let cache = C.make 32 in
  let n1, n2 = (2, 1) in

  let lists =
    [ (List.init n1 (fun i -> i), ref []); (List.init n2 (fun i -> i), ref []) ]
  in
  List.iter
    (fun (puts, gets) ->
      Atomic.spawn (fun () ->
          List.iter
            (fun elem ->
              C.put cache elem elem;
              gets := Option.get (C.get cache elem) :: !gets)
            puts)
      |> ignore)
    lists;

  Atomic.final (fun () ->
      let gets1 = !(List.nth lists 0 |> snd) in
      let gets2 = !(List.nth lists 1 |> snd) in

      Atomic.check (fun () -> List.length gets1 = n1);
      Atomic.check (fun () -> List.length gets2 = n2);

      Atomic.check (fun () ->
          let l1 = List.filter (fun i -> i < n1) gets1 in
          let l2 = List.filter (fun i -> i >= n1) gets1 in
          let l3 = List.filter (fun i -> i < n2) gets2 in
          let l4 = List.filter (fun i -> i >= n2) gets2 in
          let is_sorted l = List.sort (fun a b -> -compare a b) l = l in
          is_sorted l1 && is_sorted l2 && is_sorted l3 && is_sorted l4))

let () =
  let open Alcotest in
  run "lru_dscheck"
    [
      ( "basic",
        [
          test_case "1-put-1-get" `Quick put_get;
          test_case "1-put-1-remove" `Quick put_remove;
          test_case "2-puts" `Quick two_puts;
          test_case "2-domains" `Quick two_domains;
        ] );
    ]

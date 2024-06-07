open Kcas

module SK = struct
  type t = string

  let equal (a : string) b = String.equal a b
  let hash (i : string) = Hashtbl.hash i
  let weight _ = 1
  let pp = Fmt.string
end

module M = Lru.Make (SK) (SK)
module C = M.Tx
module S = Set.Make (String)

let add_to_cache c k = Xt.commit { tx = C.put c k k }

let get_from_cache c k =
  match Xt.commit { tx = C.get c k } with
  | None -> failwith ("Cache was missing key: " ^ k)
  | Some v when v = k -> ()
  | Some v -> failwith (Fmt.str "Cache key %s had value %s" k v)

let remove_from_cache c k =
  let _ = Xt.commit { tx = C.remove c k } in
  ()

let tests_sequential =
  QCheck.
    [
      (* Test 1: add *)
      Test.make ~name:"add" (list string) (fun lpush ->
          assume (lpush <> []);
          let cache = M.make (List.length lpush * 2) in
          List.iter (add_to_cache cache) lpush;
          assert (not (Xt.commit { tx = C.is_empty cache }));
          List.iter (get_from_cache cache) lpush;
          true);
      (* Test 2: remove *)
      Test.make ~name:"remove" (list string) (fun lpush ->
          assume (lpush <> []);
          let cache = M.make (List.length lpush * 2) in
          List.iter (add_to_cache cache) lpush;
          List.iter (remove_from_cache cache) lpush;
          assert (Xt.commit { tx = C.is_empty cache });
          true);
      (* Test 3: eviction *)
      Test.make ~name:"Eviction" (list printable_string) (fun lpush ->
          let distinct = S.of_list lpush |> S.elements in
          assume (List.length distinct >= 3);
          let cache = M.make (List.length distinct - 1) in
          let hd = List.hd distinct and tl = List.tl distinct in
          List.iter (add_to_cache cache) tl;
          let hd' = List.hd tl and tl' = List.tl tl in
          List.iter (get_from_cache cache) tl';
          add_to_cache cache hd;
          not (Xt.commit { tx = C.get cache hd' } |> Option.is_some));
    ]

let tests_one_consumer_one_producer =
  QCheck.
    [
      Test.make ~name:"parallel add-get" (list string) (fun lpush ->
          let cache = M.make (List.length lpush * 2) in
          let barrier = Barrier.create 2 in

          let producer =
            Domain.spawn (fun () ->
                Barrier.await barrier;
                List.iter (add_to_cache cache) lpush)
          in

          Barrier.await barrier;

          let verify =
            List.fold_left
              (fun acc item ->
                let rec v () =
                  match Xt.commit { tx = C.get cache item } with
                  | None ->
                      Domain.cpu_relax ();
                      v ()
                  | Some i -> acc && SK.equal i item
                in
                v ())
              true lpush
          in
          Domain.join producer;
          verify);
    ]

let () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Lru"
    [
      ("tests_sequential", to_alcotest tests_sequential);
      ( "tests_one_consumer_one_producer",
        to_alcotest tests_one_consumer_one_producer );
    ]

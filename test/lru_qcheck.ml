open Kcas
module C = Tiny_lfu.Lru.Make (String)
module S = Set.Make (String)

let add_to_cache c k = Xt.commit { tx = C.put c k k }

let get_from_cache c k =
  match Xt.commit { tx = C.get c k } with
  | None -> failwith ("Cache was missing key: " ^ k)
  | Some v when v = k -> ()
  | Some v -> failwith (Fmt.str "Cache key %s had value %s" k v)

let remove_from_cache c k = Xt.commit { tx = C.delete c k }

let tests_sequential =
  QCheck.
    [
      (* Test 1: add *)
      Test.make ~name:"add" (list string) (fun lpush ->
          assume (lpush <> []);
          let cache = C.make (List.length lpush * 2) in
          List.iter (add_to_cache cache) lpush;
          assert (not (Xt.commit { tx = C.is_empty cache }));
          List.iter (get_from_cache cache) lpush;
          true);
      (* Test 2: remove *)
      Test.make ~name:"remove" (list string) (fun lpush ->
          assume (lpush <> []);
          let cache = C.make (List.length lpush * 2) in
          List.iter (add_to_cache cache) lpush;
          List.iter (remove_from_cache cache) lpush;
          assert (Xt.commit { tx = C.is_empty cache });
          true);
      Test.make ~name:"Eviction" (list string) (fun lpush ->
          let distinct = S.of_list lpush |> S.elements in
          assume (List.length distinct >= 3);
          let cache = C.make (List.length distinct - 1) in
          let hd = List.hd distinct and tl = List.tl distinct in
          List.iter (add_to_cache cache) tl;
          let hd' = List.hd tl and tl' = List.tl tl in
          List.iter (get_from_cache cache) tl';
          add_to_cache cache hd;
          not (Xt.commit { tx = C.get cache hd' } |> Option.is_none));
    ]

let () =
  let to_alcotest = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "Lru" [ ("tests_sequential", to_alcotest tests_sequential) ]

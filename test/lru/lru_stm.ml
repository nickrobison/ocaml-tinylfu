open QCheck
open STM

module S = struct
  type t = string

  let equal (a : string) b = a = b
  let hash (i : string) = Hashtbl.hash i
  let pp = Fmt.string
end

module I = struct
  type t = int

  let weight _ = 1
  let pp = Fmt.int
end

module C = Lru.Make (S) (I)
module M = Map.Make (String)

module Spec = struct
  type cmd =
    | Add of string * int
    | Get of string
    | Remove of string
    | Size
    | Is_empty
  [@@deriving show]

  type state = int M.t
  type sut = (string, int) C.t

  let arb_cmd _s =
    let int_gen = Gen.nat in
    let str_gen = Gen.string_size (Gen.return 5) in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.map2 (fun k v -> Add (k, v)) str_gen int_gen;
           Gen.map (fun i -> Get i) str_gen;
           Gen.map (fun i -> Remove i) str_gen;
           Gen.return Size;
           Gen.return Is_empty;
         ])

  let init_state = M.empty
  let init_sut () = C.make 10000
  let cleanup _ = ()

  let next_state cmd s =
    match cmd with
    | Add (k, v) -> M.add k v s
    | Get _ -> s
    | Remove k -> M.remove k s
    | Size | Is_empty -> s

  let precond _ _ = true

  let run cmd cache =
    match cmd with
    | Add (k, v) -> Res (unit, C.put cache k v)
    | Get k -> Res (option int, C.get cache k)
    | Remove k -> Res (unit, C.remove cache k)
    | Size -> Res (int, C.size cache)
    | Is_empty -> Res (bool, C.is_empty cache)

  let postcond cmd (s : state) res =
    match (cmd, res) with
    | Is_empty, Res ((Bool, _), res) -> res = (M.cardinal s = 0)
    | Size, Res ((Int, _), res) -> res = M.cardinal s
    | Add (_, _), Res ((Unit, _), _) -> true
    | Get k, Res ((Option Int, _), res) ->
        Option.fold ~none:true ~some:(fun i -> M.find k s = i) res
    | Remove k, Res ((Unit, _), _) -> not (M.mem k s)
    | _, _ -> false
end

let () =
  Stm_run.run ~count:500 ~verbose:true ~name:"LRU tests" (module Spec) |> exit

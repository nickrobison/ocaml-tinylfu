open Cache_intf
open Kcas

module Make (K : Key) (V : Value) = struct
  type k = K.t
  type v = V.t

  module L = Lru.Make (K) (V)

  type t = { hot : L.t; cold : L.t; capacity : int }

  let make ~capacity ~split () =
    let hot_cap = capacity * split in
    let cold_cap = capacity - hot_cap in
    let hot = L.make hot_cap and cold = L.make cold_cap in
    { hot; cold; capacity }

  let capacity t = t.capacity

  module Tx = struct
    let is_empty ~xt t = L.Tx.is_empty ~xt t.hot && L.Tx.is_empty ~xt t.cold
    let is_full ~xt t = L.Tx.is_full ~xt t.hot && L.Tx.is_full ~xt t.cold
    let size ~xt t = L.Tx.size ~xt t.hot + L.Tx.size ~xt t.cold

    let get ~xt t k =
      match L.Tx.get ~xt t.hot k with
      | Some hot_v -> Some hot_v
      | None -> (
          match L.Tx.get ~xt t.cold k with
          | None ->
              None
              (*We need to move this to the hot cache, and the old one to the cold cache*)
          | Some cold_v -> (
              let _ = L.Tx.remove ~xt t.cold k in
              match L.Tx.put ~xt t.hot k cold_v with
              | None -> Some cold_v
              | Some (ev_k, ev_v) ->
                  (* There should never be anything evicted, since we're simply swapping values around*)
                  let _ = L.Tx.put ~xt t.cold ev_k ev_v in
                  Some cold_v))

    let put ~xt t k v =
      match L.Tx.put ~xt t.hot k v with
      | None -> None
      | Some (ev_k, ev_v) -> L.Tx.put ~xt t.cold ev_k ev_v

    let remove ~xt t k =
      match L.Tx.remove ~xt t.hot k with
      | true -> true
      | false -> L.Tx.remove ~xt t.cold k

    let mem ~xt t k =
      match L.Tx.mem ~xt t.hot k with
      | true -> true
      | false -> L.Tx.mem ~xt t.cold k
  end

  let is_empty t = Xt.commit { tx = Tx.is_empty t }
  let size t = Xt.commit { tx = Tx.size t }
  let get t k = Xt.commit { tx = Tx.get t k }
  let is_full t = Xt.commit { tx = Tx.is_full t }
  let put ?timeoutf t k v = Xt.commit ?timeoutf { tx = Tx.put t k v }
  let mem t k = Xt.commit { tx = Tx.mem t k }
  let remove t k = Xt.commit { tx = Tx.remove t k }
end

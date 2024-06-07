open Kcas
open Kcas_data
open Cache_intf

module Make (K : Key) (V : Value) = struct
  type k = K.t
  type v = V.t

  type t = {
    capacity : int;
    size : int Loc.t;
    table : (k, k Dllist.node * v) Hashtbl.t;
    order : k Dllist.t;
  }

  let make capacity =
    {
      capacity;
      size = Loc.make 0;
      table = Hashtbl.create ();
      order = Dllist.create ();
    }

  let capacity cache = cache.capacity

  module Tx = struct
    let is_empty ~xt cache = Xt.get ~xt cache.size == 0
    let is_full ~xt cache = Xt.get ~xt cache.size >= cache.capacity
    let size ~xt cache = Xt.get ~xt cache.size

    let remove_and_get ~xt cache k =
      match Hashtbl.Xt.find_opt ~xt cache k with
      | None -> None
      | Some (_, v) ->
          Hashtbl.Xt.remove ~xt cache k;
          Some (k, v)

    let handle_eviction ~xt t =
      if t.capacity = Xt.update ~xt t.size (fun n -> n + 1) then
        let evict_key = Dllist.Xt.take_blocking_r ~xt t.order in
        remove_and_get ~xt t.table evict_key
      else None

    let put ~xt cache k v =
      let node, evv =
        match Hashtbl.Xt.find_opt ~xt cache.table k with
        | None ->
            let evicted = handle_eviction ~xt cache in
            let n' = Dllist.Xt.add_l ~xt k cache.order in
            (n', evicted)
        | Some (node, _) ->
            Dllist.Xt.move_l ~xt node cache.order;
            (node, None)
      in
      Hashtbl.Xt.replace ~xt cache.table k (node, v);
      evv

    let get ~xt cache (k : 'k) =
      Hashtbl.Xt.find_opt ~xt cache.table k
      |> Option.map @@ fun (node, value) ->
         Dllist.Xt.move_l ~xt node cache.order;
         value

    let remove ~xt cache k =
      match Hashtbl.Xt.find_opt ~xt cache.table k with
      | None -> false
      | Some (node, _) ->
          Hashtbl.Xt.remove ~xt cache.table k;
          Dllist.Xt.remove ~xt node;
          Xt.decr ~xt cache.size;
          true

    let mem ~xt cache k = Hashtbl.Xt.mem ~xt cache.table k
  end

  let is_empty cache = Kcas.Xt.commit { tx = Tx.is_empty cache }
  let is_full cache = Kcas.Xt.commit { tx = Tx.is_full cache }
  let size cache = Kcas.Xt.commit { tx = Tx.size cache }
  let get cache k = Kcas.Xt.commit { tx = Tx.get cache k }

  let put ?timeoutf cache k v =
    Kcas.Xt.commit ?timeoutf { tx = Tx.put cache k v }

  let remove cache k = Kcas.Xt.commit { tx = Tx.remove cache k }
  let mem cache k = Kcas.Xt.commit { tx = Tx.mem cache k }
end

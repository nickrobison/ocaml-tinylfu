open Kcas
open Kcas_data

module Make (K : Stdlib.Hashtbl.HashedType) = struct
  type k = K.t
  type v = string

  type t = {
    capacity : int Loc.t;
    size : int Loc.t;
    table : (k, k Dllist.node * v) Hashtbl.t;
    order : k Dllist.t;
  }

  let make capacity =
    {
      capacity = Loc.make capacity;
      size = Loc.make 0;
      table = Hashtbl.create ();
      order = Dllist.create ();
    }

  let is_empty cache ~xt = Xt.get ~xt cache.size == 0
  let size cache ~xt = Xt.get ~xt cache.size
  let capacity cache ~xt = Xt.get ~xt cache.capacity

  let put cache ~xt k v =
    let node =
      match Hashtbl.Xt.find_opt ~xt cache.table k with
      | None ->
          let c = Xt.get ~xt cache.capacity in
          if c = Xt.update ~xt cache.size (fun n -> max 0 (n + 1)) then
            Dllist.Xt.take_blocking_r ~xt cache.order
            |> Hashtbl.Xt.remove ~xt cache.table;
          Dllist.Xt.add_l ~xt k cache.order
      | Some (node, _) ->
          Dllist.Xt.move_l ~xt node cache.order;
          node
    in
    Hashtbl.Xt.replace ~xt cache.table k (node, v)

  let get cache ~xt k =
    Hashtbl.Xt.find_opt ~xt cache.table k
    |> Option.map @@ fun (node, value) ->
       Dllist.Xt.move_l ~xt node cache.order;
       value

  let delete cache ~xt k =
    match Hashtbl.Xt.find_opt ~xt cache.table k with
    | None -> ()
    | Some (node, _) ->
        Hashtbl.Xt.remove ~xt cache.table k;
        Dllist.Xt.remove ~xt node;
        Xt.decr ~xt cache.size
end

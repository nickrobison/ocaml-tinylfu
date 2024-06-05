open Kcas
open Kcas_data

module Make (K : Stdlib.Hashtbl.HashedType) = struct
  type k = K.t
  type v = K.t

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

  let is_empty ~xt cache = Xt.get ~xt cache.size == 0
  let size cache ~xt = Xt.get ~xt cache.size
  let capacity cache ~xt:_ = cache.capacity

  let put ~xt cache k v =
    let node =
      match Hashtbl.Xt.find_opt ~xt cache.table k with
      | None ->
          if cache.capacity = Xt.update ~xt cache.size (fun n -> max 0 (n + 1))
          then
            Dllist.Xt.take_blocking_r ~xt cache.order
            |> Hashtbl.Xt.remove ~xt cache.table;
          Dllist.Xt.add_l ~xt k cache.order
      | Some (node, _) ->
          Dllist.Xt.move_l ~xt node cache.order;
          node
    in
    Hashtbl.Xt.replace ~xt cache.table k (node, v)

  let get ~xt cache k =
    Hashtbl.Xt.find_opt ~xt cache.table k
    |> Option.map @@ fun (node, value) ->
       Dllist.Xt.move_l ~xt node cache.order;
       value

  let delete ~xt cache k =
    match Hashtbl.Xt.find_opt ~xt cache.table k with
    | None -> ()
    | Some (node, _) ->
        Hashtbl.Xt.remove ~xt cache.table k;
        Dllist.Xt.remove ~xt node;
        Xt.decr ~xt cache.size
end

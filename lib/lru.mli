module Make (K : Hashtbl.HashedType) : Lru_intf.S with type k = K.t

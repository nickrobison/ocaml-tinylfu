module type Key = sig
  include Stdlib.Hashtbl.HashedType

  val pp : Format.formatter -> t -> unit
end

module type Value = sig
  type t

  val weight : t -> int
  val pp : Format.formatter -> t -> unit
end

module Make (K : Key) (V : Value) :
  Lru_intf.S with type k = K.t and type v = V.t

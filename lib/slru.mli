open Cache_intf

module Make (K : Key) (V : Value) : sig
  include S with type k = K.t and type v = V.t

  val make : capacity:int -> split:int -> unit -> t
end

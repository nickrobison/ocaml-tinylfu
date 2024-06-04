module type S = sig
  type t
  type k
  type v

  val make : int -> t
  val is_empty : t -> xt:'a Kcas.Xt.t -> bool
  val size : t -> xt:'a Kcas.Xt.t -> int
  val capacity : t -> xt:'a Kcas.Xt.t -> int
  val put : t -> xt:'a Kcas.Xt.t -> k -> v -> unit
  val get : t -> xt:'a Kcas.Xt.t -> k -> v option
  val delete : t -> xt:'a Kcas.Xt.t -> k -> unit
end

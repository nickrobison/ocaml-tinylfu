module type S = sig
  type t
  type k
  type v

  val make : int -> t
  val is_empty : xt:'a Kcas.Xt.t -> t -> bool
  val size : xt:'a Kcas.Xt.t -> t -> int
  val capacity : t -> xt:'a Kcas.Xt.t -> int
  val put : xt:'a Kcas.Xt.t -> t -> k -> v -> unit
  val get : xt:'a Kcas.Xt.t -> t -> k -> v option
  val delete : xt:'a Kcas.Xt.t -> t -> k -> unit
end

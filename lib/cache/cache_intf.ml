open Kcas

module type Ops = sig
  type k
  type v
  type t
  type ('x, 'fn) fn
  type ('x, 'fn) blocking_fn

  val is_empty : ('x, t -> bool) fn
  val size : ('x, t -> int) fn
  val get : ('x, t -> k -> v option) fn
  val put : ('x, t -> k -> v -> unit) blocking_fn
  val remove : ('x, t -> k -> bool) fn
  val mem : ('x, t -> k -> bool) fn
end

module type S = sig
  type k
  type v
  type t

  module Tx : sig
    include
      Ops
        with type k := k
        with type v := v
        with type t := t
        with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn
        with type ('x, 'fn) blocking_fn := xt:'x Xt.t -> 'fn
  end

  include
    Ops
      with type k := k
      with type v := v
      with type t := t
      with type ('x, 'fn) fn := 'fn
      with type ('x, 'fn) blocking_fn := ?timeoutf:float -> 'fn

  val make : int -> t
  val capacity : t -> int
end

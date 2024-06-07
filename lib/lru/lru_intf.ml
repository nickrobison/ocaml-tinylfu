open Kcas

module type Ops = sig
  type ('k, 'v) t
  type ('x, 'fn) fn
  type ('x, 'fn) blocking_fun

  val is_empty : ('x, ('k, 'v) t -> bool) fn
  val size : ('x, ('k, 'v) t -> int) fn
  val get : ('x, ('k, 'v) t -> 'k -> 'v option) fn
  val put : ('x, ('k, 'v) t -> 'k -> 'v -> unit) blocking_fun
  val remove : ('x, ('k, 'v) t -> 'k -> unit) fn
end

module type S = sig
  type k
  type v
  type ('k, 'v) t

  module Tx : sig
    include
      Ops
        with type ('k, 'v) t := (k, v) t
        with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn
        with type ('x, 'fn) blocking_fun := xt:'x Xt.t -> 'fn
  end

  include
    Ops
      with type ('k, 'v) t := (k, v) t
      with type ('x, 'fn) fn := 'fn
      with type ('x, 'fn) blocking_fun := ?timeoutf:float -> 'fn

  val make : int -> (k, v) t
end

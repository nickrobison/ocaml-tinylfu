type t

val make : int -> t
val sample_size : t -> int
val table : t -> int array
val block_mask : t -> int
val size : t -> int
val increment : t -> entry:int -> t
val frequency : t -> entry:int -> int
val reset : t -> t

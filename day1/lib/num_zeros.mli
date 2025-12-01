(** num_zeros.mli *)

type dir = Left | Right
type inst = { turn_dir : dir; clicks : int }

val of_str : string -> inst
val num_zero_points : inst list -> start:int -> dial_size:int -> int
val num_zero_clicks : inst list -> start:int -> dial_size:int -> int

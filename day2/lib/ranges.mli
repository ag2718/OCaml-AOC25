(** ranges.mli *)

val get_range_tuple : string -> int * int
val check_repeat_n : int -> string -> bool
val check_any_repeat : string -> bool
val sum_ids_with_double_repeat : int * int -> int
val sum_ids_with_any_repeat : int * int -> int

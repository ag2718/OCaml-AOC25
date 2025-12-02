(** ranges.mli *)

val get_range_tuple : string -> int * int
(** Repeat check functions for individual IDs *)
val check_repeat_n : int -> string -> bool
val check_any_repeat : string -> bool
(** Range sums for parts 1 and 2 *)
val sum_ids_with_double_repeat : int * int -> int
val sum_ids_with_any_repeat : int * int -> int

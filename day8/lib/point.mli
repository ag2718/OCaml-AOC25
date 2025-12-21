open Core

module PointKey : sig
  type t = int * int * int [@@deriving compare, sexp_of, hash]

  include Comparator.S with type t := t
end

type point = PointKey.t

val sqdist : point -> point -> int
val order : point -> point -> bool

open Core

module PointKey = struct
  module T = struct
    type t = int * int * int [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)
end

type point = PointKey.t

let order ((x1, y1, z1) : point) ((x2, y2, z2) : point) =
  x1 * y1 * z1 > x2 * y2 * z2

let sqdist ((x1, y1, z1) : point) ((x2, y2, z2) : point) =
  ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2)) + ((z1 - z2) * (z1 - z2))

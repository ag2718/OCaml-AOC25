open Core
open Point

type circuit = (PointKey.t, PointKey.comparator_witness) Set.t

let empty_circuits boxes : circuit list =
  List.map boxes ~f:(fun box -> Set.of_list (module PointKey) [ box ])

let update_circuits circuits (b1, b2) =
  let find_circuit_for_box (box : point) =
    List.find_mapi_exn circuits ~f:(fun i circuit ->
        if Set.mem circuit box then Some (i, circuit) else None)
  in
  let i1, c1 = find_circuit_for_box b1 in
  let i2, c2 = find_circuit_for_box b2 in
  if i1 = i2 then circuits
  else
    let combined = Set.union c1 c2 in
    let new_circuits =
      combined :: List.filteri circuits ~f:(fun i _ -> not (i = i1 || i = i2))
    in
    new_circuits

let get_circuits_list circuit_list = List.map circuit_list ~f:Set.to_list

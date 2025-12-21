open Core
open Point

let parse_input lines =
  List.map lines ~f:(fun line ->
      let nums =
        line |> String.split_on_chars ~on:[ ',' ] |> List.map ~f:Int.of_string
      in
      match nums with
      | [ x; y; z ] -> (x, y, z)
      | _ -> failwithf "Invalid line %s" line ())

let get_sorted_edges points =
  List.cartesian_product points points
  |> List.filter ~f:(fun (b1, b2) -> not ([%compare.equal: PointKey.t] b1 b2))
  |> List.filter ~f:(fun (b1, b2) -> order b1 b2)
  |> List.map ~f:(fun (b1, b2) -> ((b1, b2), sqdist b1 b2))
  |> List.sort ~compare:(fun (_, d1) (_, d2) -> compare d1 d2)
  |> List.map ~f:fst

let get_num_circuits (points : point list) =
  let edges = List.take (get_sorted_edges points) 1000 in
  let disconnected = Circuits.empty_circuits points in
  List.fold edges ~init:disconnected ~f:Circuits.update_circuits
  |> Circuits.get_circuits_list |> List.map ~f:List.length
  |> List.sort ~compare:(fun x y -> compare y x)
  |> Fn.flip List.take 3 |> List.fold ~init:1 ~f:( * )

let get_last_connection (points : point list) =
  let edges = get_sorted_edges points in
  List.fold_until edges
    ~init:(Circuits.empty_circuits points)
    ~f:(fun circuits edge ->
      let new_circuits = Circuits.update_circuits circuits edge in
      match new_circuits with [ _ ] -> Stop edge | _ -> Continue new_circuits)
    ~finish:(fun _ -> assert false)

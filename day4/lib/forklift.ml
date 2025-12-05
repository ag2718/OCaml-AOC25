open Core

let get_neighbors grid (r, c) =
  let m, n = (Array.length grid, Array.length grid.(0)) in
  let adj_indices =
    List.cartesian_product [ -1; 0; 1 ] [ -1; 0; 1 ]
    |> List.filter ~f:(fun (dr, dc) -> not (dr = 0 && dc = 0))
    |> List.map ~f:(fun (dr, dc) -> (r + dr, c + dc))
  in
  List.filter_map adj_indices ~f:(fun (nr, nc) ->
      if nr >= 0 && nr < m && nc >= 0 && nc < n then Some grid.(nr).(nc)
      else None)

let num_adj_rolls grid (r, c) =
  let neighbors = get_neighbors grid (r, c) in
  List.fold neighbors ~init:0 ~f:(fun cnt n ->
      if Char.equal n '@' then cnt + 1 else cnt)

let get_rolls_with_few_neighbors grid =
  let m, n = (Array.length grid, Array.length grid.(0)) in
  let indices = List.init (m * n) ~f:(fun i -> (i / n, i % n)) in
  List.filter_map indices ~f:(fun (r, c) ->
      let cell = grid.(r).(c) in
      let num_adj = num_adj_rolls grid (r, c) in
      if Char.equal cell '@' && num_adj < 4 then Some (r, c) else None)

let count_iterative_removal grid =
  let rec aux acc =
    let to_remove = get_rolls_with_few_neighbors grid in
    if List.length to_remove = 0 then acc
    else (
      List.iter to_remove ~f:(fun (r, c) -> grid.(r).(c) <- '.');
      aux (acc + List.length to_remove))
  in
  aux 0

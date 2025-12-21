open Core

let parse_lines lines =
  List.map lines ~f:(fun line ->
      match String.split_on_chars ~on:[ ',' ] line with
      | [ c; r ] -> (Int.of_string r, Int.of_string c)
      | _ -> failwithf "Invalid line %s" line ())

let largest_area_red_corners red_tiles =
  let tile_pairs = List.cartesian_product red_tiles red_tiles in
  let area, ((r1, c1), (r2, c2)) =
    List.fold tile_pairs
      ~init:(0, ((0, 0), (0, 0)))
      ~f:(fun (res, rect) (pt1, pt2) ->
        let a = Grid.area pt1 pt2 in
        if a > res then (a, (pt1, pt2)) else (res, rect))
  in
  printf "Rect (%d, %d) - (%d, %d)\n" c1 r1 c2 r2;
  area

let largest_area_filled red_tiles =
  let tile_pairs = List.cartesian_product red_tiles red_tiles in
  let edges =
    let red_tiles_arr = Array.of_list red_tiles in
    List.init (List.length red_tiles) ~f:(fun i ->
        (red_tiles_arr.(i), red_tiles_arr.((i + 1) % List.length red_tiles)))
  in
  let area, ((r1, c1), (r2, c2)) =
    List.fold tile_pairs
      ~init:(0, ((0, 0), (0, 0)))
      ~f:(fun (a, rect) (pt1, pt2) ->
        if Grid.check_rect_filled edges pt1 pt2 then
          let area = Grid.area pt1 pt2 in
          if a > area then (a, rect) else (area, (pt1, pt2))
        else (a, rect))
  in
  printf "Rect (%d, %d) - (%d, %d)\n" c1 r1 c2 r2;
  area

open Core

let parse_manifold (map : char list list) =
  let start_pos =
    List.find_mapi_exn map ~f:(fun r row ->
        List.find_mapi row ~f:(fun c cell ->
            if Char.equal 'S' cell then Some (r, c) else None))
  in
  let splitter_map =
    List.map map ~f:(fun row ->
        List.map row ~f:(fun cell -> Char.equal cell '^'))
  in
  (start_pos, splitter_map)

let count_splits (start_r, start_c) splitter_map =
  let n = splitter_map |> List.hd_exn |> List.length in
  let bottom_rows = List.drop splitter_map (start_r + 1) in
  let beams = List.init n ~f:(fun i -> i = start_c) in
  let cnt, _ =
    List.fold bottom_rows ~init:(0, beams)
      ~f:(fun (cnt, curr_beams) splitters ->
        let to_split = List.map2_exn curr_beams splitters ~f:( && ) in
        let new_beams =
          List.init n ~f:(fun i ->
              let from_above = List.nth_exn curr_beams i in
              let from_to_split =
                (i > 0 && List.nth_exn to_split (i - 1))
                || (i < n - 1 && List.nth_exn to_split (i + 1))
              in
              (from_above || from_to_split) && not (List.nth_exn splitters i))
        in
        (cnt + List.count to_split ~f:Fn.id, new_beams))
  in
  cnt

let count_quantum_splits (start_r, start_c) splitter_map =
  let n = splitter_map |> List.hd_exn |> List.length in
  let bottom_rows = List.drop splitter_map (start_r + 1) in
  let beams = List.init n ~f:(fun i -> if i = start_c then 1 else 0) in
  let final_row_traj_counts =
    List.fold bottom_rows ~init:beams ~f:(fun curr_beams splitters ->
        List.init n ~f:(fun i ->
            let from_above =
              if List.nth_exn splitters i then 0 else List.nth_exn curr_beams i
            in
            let from_left =
              if i > 0 && List.nth_exn splitters (i - 1) then
                List.nth_exn curr_beams (i - 1)
              else 0
            in
            let from_right =
              if i < n - 1 && List.nth_exn splitters (i + 1) then
                List.nth_exn curr_beams (i + 1)
              else 0
            in
            from_above + from_left + from_right))
  in
  List.fold final_row_traj_counts ~init:0 ~f:( + )

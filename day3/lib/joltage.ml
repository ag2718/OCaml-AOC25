open Core

let get_battery_list s =
  String.fold s ~init:[] ~f:(fun acc c ->
      (Char.to_int c - Char.to_int '0') :: acc)
  |> List.rev

let get_largest_joltage n batteries =
  let rec aux acc n batteries len =
    if n = 0 then acc
    else
      match
        List.foldi batteries ~init:None ~f:(fun i acc b ->
            let enough_bats = i + n <= len in
            match acc with
            | None -> Some (i, b)
            | Some (_, mb) when b > mb && enough_bats -> Some (i, b)
            | Some _ -> acc)
      with
      | None ->
          failwith
            (sprintf
               "get_largest_joltage: insufficient digits (need %d, have %d)" n
               len)
      | Some (bat_idx, bat_val) ->
          aux
            ((acc * 10) + bat_val)
            (n - 1)
            (List.drop batteries (bat_idx + 1))
            (len - (bat_idx + 1))
  in
  aux 0 n batteries (List.length batteries)

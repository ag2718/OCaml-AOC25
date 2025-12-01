open Core

type dir = Left | Right
type inst = { turn_dir : dir; clicks : int }

let of_str s =
  let turn_dir =
    match String.get s 0 with
    | 'L' -> Left
    | 'R' -> Right
    | c -> failwithf "Unexpected direction character: %c" c ()
  in
  let clicks = String.subo s ~pos:1 |> Int.of_string in
  { turn_dir; clicks }

let update_pos { turn_dir; clicks } ~pos ~dial_size =
  let no_mod_sum =
    match turn_dir with Left -> pos - clicks | Right -> pos + clicks
  in
  no_mod_sum % dial_size

let num_zero_points insts ~start ~dial_size =
  List.fold insts ~init:(start, 0) ~f:(fun (pos, cnt) inst ->
      let new_pos = update_pos inst ~pos ~dial_size in
      let new_cnt = if new_pos = 0 then cnt + 1 else cnt in
      (new_pos, new_cnt))
  |> snd

let zero_clicks_for_turn pos { turn_dir; clicks } ~dial_size =
  let dist_to_zero =
    match turn_dir with Left -> pos | Right -> dial_size - pos
  in
  (clicks / dial_size)
  + if 0 < dist_to_zero && dist_to_zero <= clicks mod dial_size then 1 else 0

let num_zero_clicks insts ~start ~dial_size =
  List.fold insts ~init:(start, 0) ~f:(fun (pos, cnt) inst ->
      let new_pos = update_pos inst ~pos ~dial_size in
      let new_cnt = cnt + zero_clicks_for_turn pos inst ~dial_size in
      (new_pos, new_cnt))
  |> snd

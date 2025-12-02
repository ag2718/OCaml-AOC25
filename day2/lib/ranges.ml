open Core

let get_range_tuple range_str =
  match String.split_on_chars ~on:[ '-' ] range_str with
  | [ start_rng; end_rng ] ->
      let start_int = Int.of_string start_rng in
      let end_int = Int.of_string end_rng in
      (start_int, end_int)
  | _ -> failwithf "Range string %s is not correctly formatted!" range_str ()

let check_repeat_n n id =
  let str_len = String.length id in
  if str_len % n <> 0 then false
  else
    let repeat_len = str_len / n in
    let repeat = String.sub id ~pos:0 ~len:repeat_len in
    let rec aux pos =
      if pos = str_len then true
      else
        let current_chunk = String.sub id ~pos ~len:repeat_len in
        if String.equal current_chunk repeat then aux (pos + repeat_len)
        else false
    in
    aux repeat_len

let check_any_repeat id =
  let rec aux n =
    if n > String.length id then false
    else if check_repeat_n n id then true
    else aux (n + 1)
  in
  aux 2

let sum_ids_on_cond ~f (start_rng, end_rng) =
  let rec aux s e cnt =
    if s > e then cnt
    else
      let new_cnt = if f (Int.to_string s) then cnt + s else cnt in
      aux (s + 1) e new_cnt
  in
  aux start_rng end_rng 0

let sum_ids_with_double_repeat (start_rng, end_rng) =
  sum_ids_on_cond ~f:(fun id -> check_repeat_n 2 id) (start_rng, end_rng)

let sum_ids_with_any_repeat (start_rng, end_rng) =
  sum_ids_on_cond ~f:check_any_repeat (start_rng, end_rng)

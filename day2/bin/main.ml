open Core
open Day2

let () =
  let filename =
    match Sys.get_argv () with
    | [| _prog; filename |] -> filename
    | _ -> failwith "Usage: [exec cmd] <input_file>"
  in
  let input =
    In_channel.read_all filename
    |> String.filter ~f:(fun c -> not (Char.equal c '\n'))
  in
  let ranges =
    String.split_on_chars ~on:[ ',' ] input
    |> List.map ~f:Ranges.get_range_tuple
  in
  let ans1 =
    List.fold ranges ~init:0 ~f:(fun cnt range ->
        cnt + Ranges.sum_ids_with_double_repeat range)
  in
  printf "Part 1: %d\n" ans1;
  let ans2 =
    List.fold ranges ~init:0 ~f:(fun cnt range ->
        cnt + Ranges.sum_ids_with_any_repeat range)
  in
  printf "Part 2: %d\n" ans2

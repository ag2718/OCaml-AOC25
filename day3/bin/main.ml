open Core
open Day3

let () =
  let filename =
    match Sys.get_argv () with
    | [| _prog; filename |] -> filename
    | _ -> failwith "Usage: [exec cmd] <input_file>"
  in
  let input_lines = In_channel.read_lines filename in
  let ans1 =
    List.fold input_lines ~init:0 ~f:(fun acc line ->
        acc + (line |> Joltage.get_battery_list |> Joltage.get_largest_joltage 2))
  in
  printf "Part 1: %d\n" ans1;
  let ans2 =
    List.fold input_lines ~init:0 ~f:(fun acc line ->
        acc
        + (line |> Joltage.get_battery_list |> Joltage.get_largest_joltage 12))
  in
  printf "Part 2: %d\n" ans2

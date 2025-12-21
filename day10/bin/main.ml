open Core
open Day10.Solution

let () =
  let filename =
    match Sys.get_argv () with
    | [| _prog; filename |] -> filename
    | _ -> failwith "Usage: [exec cmd] <input_file>"
  in
  let machines = filename |> In_channel.read_lines |> List.map ~f:parse_line in
  let ans1 =
    machines |> List.map ~f:min_presses_indicators |> List.fold ~init:0 ~f:( + )
  in
  printf "Part 1: %d\n" ans1
(* let ans2 =
    machines
    |> List.map ~f:(fun m -> min_presses_joltage m.buttons m.joltage)
    |> List.fold ~init:0 ~f:( + )
  in
  printf "Part 2: %d\n" ans2 *)

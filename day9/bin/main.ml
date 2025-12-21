open Core
open Day9.Solution

let () =
  let filename =
    match Sys.get_argv () with
    | [| _prog; filename |] -> filename
    | _ -> failwith "Usage: [exec cmd] <input_file>"
  in
  let red_tiles = filename |> In_channel.read_lines |> parse_lines in
  let ans1 = largest_area_red_corners red_tiles in
  printf "Part 1: %d\n" ans1;
  let ans2 = largest_area_filled red_tiles in
  printf "Part 2: %d\n" ans2

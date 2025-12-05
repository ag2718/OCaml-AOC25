open Core
open Day5.Solution

let () =
  let filename =
    match Sys.get_argv () with
    | [| _prog; filename |] -> filename
    | _ -> failwith "Usage: [exec cmd] <input_file>"
  in
  let fresh, available =
    filename |> In_channel.read_lines |> parse_input_lines
  in
  let ans1 = get_num_fresh fresh available in
  printf "Part 1: %d\n" ans1;
  let ans2 = fresh |> merge_ranges |> count_ranges_cover in
  printf "Part 2: %d\n" ans2

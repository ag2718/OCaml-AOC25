open Core
open Day8.Solution

let () =
  let filename =
    match Sys.get_argv () with
    | [| _prog; filename |] -> filename
    | _ -> failwith "Usage: [exec cmd] <input_file>"
  in
  let points = filename |> In_channel.read_lines |> parse_input in
  let ans1 = get_num_circuits points in
  printf "Part 1: %d\n" ans1;
  let last_edge = get_last_connection points in
  let (x1, _, _), (x2, _, _) = last_edge in
  printf "Part 2: %d\n" (x1 * x2)

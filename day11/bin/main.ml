open Core
open Day11.Solution

let () =
  let filename =
    match Sys.get_argv () with
    | [| _prog; filename |] -> filename
    | _ -> failwith "Usage: [exec cmd] <input_file>"
  in
  let adj_lines = filename |> In_channel.read_lines |> List.map ~f:parse_line in
  let ans1 = num_paths adj_lines "you" "out" ~contains:[] in
  printf "Part 1: %s\n" (Int64.to_string ans1);
  let ans2 = num_paths adj_lines "svr" "out" ~contains:[ "dac"; "fft" ] in
  printf "Part 2: %s\n" (Int64.to_string ans2)

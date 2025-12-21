open Core
open Day7.Solution

let () =
  let filename =
    match Sys.get_argv () with
    | [| _prog; filename |] -> filename
    | _ -> failwith "Usage: [exec cmd] <input_file>"
  in
  let lines = In_channel.read_lines filename |> List.map ~f:String.to_list in
  let start, splitters = parse_manifold lines in
  let ans1 = count_splits start splitters in
  printf "Part 1: %d\n" ans1;
  let ans2 = count_quantum_splits start splitters in
  printf "Part 2: %d\n" ans2

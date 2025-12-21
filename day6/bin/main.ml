open Core
open Day6.Solution

let () =
  let filename =
    match Sys.get_argv () with
    | [| _prog; filename |] -> filename
    | _ -> failwith "Usage: [exec cmd] <input_file>"
  in
  let val_lines, op_line =
    match filename |> In_channel.read_lines |> List.rev with
    | hd :: tl -> (tl, hd)
    | _ -> failwith "Invalid input"
  in
  let vals1 = parse_input val_lines in
  let ops = split_on_whitespace op_line in
  let results1 = List.map2_exn vals1 ops ~f:fold_vals in
  let ans1 = List.fold results1 ~init:0 ~f:( + ) in
  printf "Part 1: %d\n" ans1;
  let vals2 = parse_input_ceph val_lines in
  let results2 = List.map2_exn vals2 ops ~f:fold_vals in
  let ans2 = List.fold results2 ~init:0 ~f:( + ) in
  printf "Part 2: %d\n" ans2

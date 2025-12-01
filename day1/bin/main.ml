open Core

let () =
  let filename =
    match Sys.get_argv () with
    | [| _prog; filename |] -> filename
    | _ -> failwith "Usage: [exec cmd] <input_file>"
  in
  let insts =
    In_channel.read_lines filename |> List.map ~f:Day1.Num_zeros.of_str
  in
  let ans1 = Day1.Num_zeros.num_zero_points insts ~start:50 ~dial_size:100 in
  printf "Part 1: %d\n" ans1;
  let ans2 = Day1.Num_zeros.num_zero_clicks insts ~start:50 ~dial_size:100 in
  printf "Part 2: %d\n" ans2

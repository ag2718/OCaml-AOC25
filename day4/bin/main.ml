open Core
open Day4.Forklift

let () =
  let filename =
    match Sys.get_argv () with
    | [| _prog; filename |] -> filename
    | _ -> failwith "Usage: [exec cmd] <input_file>"
  in
  let input = In_channel.read_lines filename in
  let grid =
    List.map input ~f:String.to_list
    |> List.map ~f:Array.of_list |> Array.of_list
  in
  let ans1 = List.length (get_rolls_with_few_neighbors grid) in
  printf "Part 1: %d\n" ans1;
  let ans2 = count_iterative_removal grid in
  printf "Part 2: %d\n" ans2

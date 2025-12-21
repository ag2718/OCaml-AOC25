open Core

let split_on_whitespace str =
  str
  |> String.split_on_chars ~on:[ ' ' ]
  |> List.filter ~f:(fun s -> s |> String.is_empty |> not)

let parse_input lines =
  let fold_line (acc : int list list option) line =
    let nums = line |> split_on_whitespace |> List.map ~f:Int.of_string in
    match acc with
    | None -> Some (List.map nums ~f:(fun x -> [ x ]))
    | Some curr -> Some (List.map2_exn curr nums ~f:(fun col n -> n :: col))
  in

  match List.fold lines ~init:None ~f:fold_line with
  | None -> failwith "No input lines!"
  | Some res -> res

let parse_input_ceph lines =
  let fold_input_col acc col =
    if col |> String.lstrip |> String.length = 0 then [] :: acc
    else
      match acc with
      | hd :: tl ->
          ((col |> String.rstrip |> String.lstrip |> String.rev |> Int.of_string)
          :: hd)
          :: tl
      | _ -> assert false
  in
  let tokens =
    List.map lines ~f:(fun x ->
        x |> String.to_list |> List.map ~f:Char.to_string)
  in
  let rec get_cols acc remaining =
    try
      let col = remaining |> List.map ~f:List.hd_exn |> String.concat in
      let rest = remaining |> List.map ~f:List.tl_exn in
      get_cols (col :: acc) rest
    with _ -> acc
  in
  List.fold (get_cols [] tokens) ~init:[ [] ] ~f:fold_input_col

let fold_vals vals op =
  match op with
  | "+" -> List.fold vals ~init:0 ~f:( + )
  | "*" -> List.fold vals ~init:1 ~f:(fun acc x -> acc * x)
  | _ -> failwithf "Invalid op %s" op ()

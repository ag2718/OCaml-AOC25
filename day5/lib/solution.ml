open Core

let parse_input_lines lines =
  let rec aux switch (fresh_ranges, available) = function
    | [] -> (fresh_ranges, available)
    | hd :: tl -> (
        if String.equal hd "" then aux true (fresh_ranges, available) tl
        else if switch then
          aux switch (fresh_ranges, Int.of_string hd :: available) tl
        else
          match String.split hd ~on:'-' with
          | [ s; e ] ->
              aux switch
                ((Int.of_string s, Int.of_string e) :: fresh_ranges, available)
                tl
          | _ -> failwithf "Invalid range argument %s" hd ())
  in
  aux false ([], []) lines

let get_num_fresh fresh_ranges available =
  let is_fresh ingredient =
    List.fold fresh_ranges ~init:false ~f:(fun fresh (s, e) ->
        fresh || (ingredient >= s && ingredient <= e))
  in
  List.fold available ~init:0 ~f:(fun count ing ->
      if is_fresh ing then count + 1 else count)

let merge_ranges ranges =
  let sorted_ranges =
    List.sort ranges ~compare:(fun (a_s, a_e) (b_s, b_e) ->
        let s_comp = compare a_s b_s in
        let e_comp = compare a_e b_e in
        if s_comp = 0 then e_comp else s_comp)
  in
  let rec aux acc curr_start curr_end = function
    | [] -> (
        match (curr_start, curr_end) with
        | None, None -> acc
        | Some curr_s, Some curr_e -> (curr_s, curr_e) :: acc
        | _ -> assert false)
    | (s, e) :: tl -> (
        match (curr_start, curr_end) with
        | None, None -> aux acc (Some s) (Some e) tl
        | Some curr_s, Some curr_e ->
            if s <= curr_e + 1 then
              aux acc (Some curr_s) (Some (max curr_e e)) tl
            else aux ((curr_s, curr_e) :: acc) (Some s) (Some e) tl
        | _ -> assert false)
  in
  aux [] None None sorted_ranges

let count_ranges_cover disjoint_ranges =
  List.fold disjoint_ranges ~init:0 ~f:(fun cover (s, e) -> cover + (e - s + 1))

open Core

module BoolState = struct
  module T = struct
    type t = bool list [@@deriving sexp, compare]
  end

  include T
  include Comparator.Make (T)
end

module IntState = struct
  module T = struct
    type t = int list [@@deriving sexp, compare]
  end

  include T
  include Comparator.Make (T)
end

type machine = {
  buttons : int list list;
  indicators : bool list;
  joltage : int list;
}

let parse_indicators s =
  String.fold s ~init:[] ~f:(fun res c ->
      match c with
      | '[' | ']' -> res
      | '.' -> false :: res
      | '#' -> true :: res
      | _ -> failwithf "Invalid char in indicator string %c" c ())
  |> List.rev

let parse_num_list s =
  s
  |> String.split_on_chars ~on:[ ','; '('; ')'; '{'; '}' ]
  |> List.filter ~f:(fun elt -> String.length elt > 0)
  |> List.map ~f:Int.of_string

let parse_line line =
  let elements = String.split_on_chars ~on:[ ' ' ] line in
  let buttons, indicators, joltage =
    List.foldi elements ~init:([], [], []) ~f:(fun i (b, ind, j) elt ->
        if i = 0 then (b, parse_indicators elt, j)
        else if i < List.length elements - 1 then
          (parse_num_list elt :: b, ind, j)
        else (b, ind, parse_num_list elt))
  in
  { buttons; indicators; joltage }

let min_presses_indicators { buttons; indicators; _ } =
  let rec bfs visited (curr_states : BoolState.t list) num_pressed =
    let state_transitions = List.cartesian_product curr_states buttons in
    let new_states =
      List.map state_transitions ~f:(fun (s, t) ->
          List.mapi s ~f:(fun i state ->
              if List.mem t i ~equal:Int.equal then not state else state))
      |> List.filter ~f:(fun x -> not (Set.mem visited x))
    in
    if List.mem new_states indicators ~equal:Poly.equal then num_pressed + 1
    else
      let new_visited =
        Set.union visited (Set.of_list (module BoolState) new_states)
      in
      bfs new_visited new_states (num_pressed + 1)
  in

  let init_state = List.init (List.length indicators) ~f:(fun _ -> false) in
  bfs (Set.empty (module BoolState)) [ init_state ] 0

let min_presses_joltage { buttons; _; joltage } =
  let rec bfs visited (curr_states : BoolState.t list) num_pressed =
    let state_transitions = List.cartesian_product curr_states buttons in
    let new_states =
      List.map state_transitions ~f:(fun (s, t) ->
          List.mapi s ~f:(fun i state ->
              if List.mem t i ~equal:Int.equal then not state else state))
      |> List.filter ~f:(fun x -> not (Set.mem visited x))
    in
    if List.mem new_states indicators ~equal:Poly.equal then num_pressed + 1
    else
      let new_visited =
        Set.union visited (Set.of_list (module BoolState) new_states)
      in
      bfs new_visited new_states (num_pressed + 1)
  in

  let init_state = List.init (List.length indicators) ~f:(fun _ -> false) in
  bfs (Set.empty (module BoolState)) [ init_state ] 0
open Core

module StringState = struct
  module T = struct
    type t = string [@@deriving sexp, compare, hash]
  end

  include T
  include Comparator.Make (T)
end

module DfsArgs = struct
  module T = struct
    type t = Set.M(StringState).t * string [@@deriving sexp, compare, hash]
  end

  include T
  include Comparator.Make (T)
end

let parse_line line =
  match String.split_on_chars ~on:[ ' ' ] line with
  | hd :: tl ->
      let src = String.sub hd ~pos:0 ~len:(String.length hd - 1) in
      (src, tl)
  | _ -> failwithf "Invalid input line %s" line ()

let num_paths adj_list_lists src dst ~contains : int64 =
  let adj_list_map = Map.of_alist_exn (module StringState) adj_list_lists in

  let memo = Hashtbl.create (module DfsArgs) in

  let rec dfs (seen : Set.M(StringState).t) (curr : string) : int64 =
    match Hashtbl.find memo (seen, curr) with
    | Some result -> result
    | None ->
        let result =
          if String.equal curr dst then
            if Set.length seen = List.length contains then 1L else 0L
          else
            let new_seen =
              if List.mem contains curr ~equal:String.equal then
                Set.add seen curr
              else seen
            in
            let children = Map.find_exn adj_list_map curr in
            List.fold children ~init:0L ~f:(fun res c ->
                Int64.(res + dfs new_seen c))
        in
        Hashtbl.set memo ~key:(seen, curr) ~data:result;
        result
  in

  dfs (Set.empty (module StringState)) src

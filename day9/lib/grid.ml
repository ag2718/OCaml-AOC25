open Core

let area (r1, c1) (r2, c2) =
  let width = 1 + abs (c1 - c2) in
  let height = 1 + abs (r1 - r2) in
  width * height

let no_edge_crosses_interior edges (r1, c1) (r2, c2) =
  let rl, cl, rg, cg = (min r1 r2, min c1 c2, max r1 r2, max c1 c2) in
  List.for_all edges ~f:(fun ((e_r1, e_c1), (e_r2, e_c2)) ->
      let e_rl, e_cl, e_rg, e_cg =
        (min e_r1 e_r2, min e_c1 e_c2, max e_r1 e_r2, max e_c1 e_c2)
      in
      let horiz_edge_intsct =
        e_rl = e_rg && (e_rl > rl && e_rl < rg) && e_cg > cl && e_cl < cg
      in
      let vert_edge_intsct =
        e_cl = e_cg && (e_cl > cl && e_cl < cg) && e_rg > rl && e_rl < rg
      in
      not (horiz_edge_intsct || vert_edge_intsct))

let check_rect_filled edges pt1 pt2 = no_edge_crosses_interior edges pt1 pt2

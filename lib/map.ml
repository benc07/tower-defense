open Game_types

type t = {
  tiles : tile array array;
  width : int;
  height : int;
  start : position;
  base : position;
}

let width m = m.width
let height m = m.height
let in_bounds m p = p.x >= 0 && p.y >= 0 && p.x < m.width && p.y < m.height
let get_tile m p = if in_bounds m p then Some m.tiles.(p.y).(p.x) else None
let can_place_tower m p = get_tile m p = Some Buildable
let start_position m = m.start
let base_position m = m.base

let of_rows rows =
  let height = List.length rows in
  let width =
    match rows with
    | [] -> 0
    | row :: _ -> List.length row
  in
  if height = 0 || width = 0 then
    Error "Map must have at least one row and column"
  else if List.exists (fun row -> List.length row <> width) rows then
    Error "Map rows must all have the same length"
  else
    let tiles = Array.of_list (List.map Array.of_list rows) in
    let starts = ref [] and bases = ref [] in
    Array.iteri
      (fun y row ->
        Array.iteri
          (fun x tile ->
            let p = { x; y } in
            if tile = Start then starts := p :: !starts;
            if tile = Base then bases := p :: !bases)
          row)
      tiles;
    match (!starts, !bases) with
    | [ start ], [ base ] -> Ok { tiles; width; height; start; base }
    | [], _ -> Error "Map is missing a spawn point (S)"
    | _, [] -> Error "Map is missing a base (B)"
    | _ :: _ :: _, _ -> Error "Map must contain exactly one spawn point (S)"
    | _, _ -> Error "Map must contain exactly one base (B)"

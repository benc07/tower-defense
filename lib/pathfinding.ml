open Game_types

let traversable map pos =
  match Map.get_tile map pos with
  | Some (Start | Path | Base) -> true
  | _ -> false

let neighbors p =
  [
    { x = p.x + 1; y = p.y };
    { x = p.x - 1; y = p.y };
    { x = p.x; y = p.y + 1 };
    { x = p.x; y = p.y - 1 };
  ]

let find_path map =
  let start = Map.start_position map and goal = Map.base_position map in
  let visited = Array.make_matrix (Map.height map) (Map.width map) false in
  let parent = Array.make_matrix (Map.height map) (Map.width map) None in
  let queue = Queue.create () in
  visited.(start.y).(start.x) <- true;
  Queue.add start queue;
  while not (Queue.is_empty queue) do
    let p = Queue.take queue in
    if not (equal_position p goal) then
      List.iter
        (fun n ->
          if traversable map n && not visited.(n.y).(n.x) then (
            visited.(n.y).(n.x) <- true;
            parent.(n.y).(n.x) <- Some p;
            Queue.add n queue))
        (neighbors p)
  done;
  if not visited.(goal.y).(goal.x) then None
  else
    let rec backtrack p acc =
      match parent.(p.y).(p.x) with
      | None -> p :: acc
      | Some prev -> backtrack prev (p :: acc)
    in
    Some (backtrack goal [])

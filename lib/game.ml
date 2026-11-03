open Game_types
type t = { map : Map.t; path : position list; enemies : Enemy.t list;
           towers : Tower.t list; gold : int; lives : int; tick : int }
let map g = g.map
let path g = g.path
let enemies g = g.enemies
let towers g = g.towers
let gold g = g.gold
let lives g = g.lives
let tick g = g.tick
let lost g = g.lives <= 0
let create map =
  match Pathfinding.find_path map with
  | None -> Error "Cannot create a game without a path to the base"
  | Some path ->
    Ok { map; path; enemies = [Enemy.create 0]; towers = [];
         gold = 100; lives = 5; tick = 0 }
let enemy_position g enemy = List.nth g.path enemy.Enemy.path_index
let place_tower g position =
  if lost g then Error "The game is over"
  else if not (Map.can_place_tower g.map position) then
    Error "Towers must be placed on a T tile"
  else if List.exists (fun tower -> equal_position tower.Tower.position position) g.towers then
    Error "There is already a tower at that position"
  else if g.gold < Tower.cost then Error "Not enough gold"
  else Ok { g with towers = g.towers @ [Tower.create position];
                   gold = g.gold - Tower.cost }
let next_tick g =
  if lost g then g
  else
    let path_length = List.length g.path in
    let moved = List.map (fun enemy -> Enemy.move enemy ~path_length) g.enemies in
    let arrived, active = List.partition (fun enemy ->
      Enemy.reached_base enemy ~path_length) moved in
    let after_movement =
      { g with enemies = active; lives = max 0 (g.lives - List.length arrived) } in
    if lost after_movement then { after_movement with tick = g.tick + 1 }
    else
      let attack_once enemies tower =
        let targets = List.filter (fun enemy ->
          Tower.in_range tower (enemy_position g enemy)) enemies in
        let ordered = List.sort (fun a b ->
          let by_progress = compare b.Enemy.path_index a.Enemy.path_index in
          if by_progress <> 0 then by_progress else compare a.Enemy.id b.Enemy.id) targets in
        match ordered with
        | [] -> enemies
        | target :: _ -> List.map (fun enemy ->
            if enemy.Enemy.id = target.Enemy.id then Tower.attack tower enemy
            else enemy) enemies
      in
      let damaged = List.fold_left attack_once active g.towers in
      let dead, survivors = List.partition Enemy.is_dead damaged in
      { after_movement with enemies = survivors;
        gold = g.gold + (List.length dead * Enemy.reward);
        tick = g.tick + 1 }

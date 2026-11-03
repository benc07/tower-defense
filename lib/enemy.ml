type t = {
  id : int;
  health : int;
  path_index : int;
}

let reward = 10
let create id = { id; health = 30; path_index = 0 }

let move enemy ~path_length =
  if path_length <= 0 then enemy
  else { enemy with path_index = min (path_length - 1) (enemy.path_index + 1) }

let take_damage enemy damage =
  { enemy with health = max 0 (enemy.health - max 0 damage) }

let is_dead enemy = enemy.health <= 0

let reached_base enemy ~path_length =
  path_length > 0 && enemy.path_index >= path_length - 1

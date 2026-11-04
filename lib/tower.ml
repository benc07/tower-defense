open Game_types

type t = {
  position : position;
  range : float;
  damage : int;
  cost : int;
}

let cost = 20

let create position =
  { position; range = 2.5; damage = 10; cost }

let in_range tower p =
  let dx = float_of_int (tower.position.x - p.x) in
  let dy = float_of_int (tower.position.y - p.y) in
  dx *. dx +. dy *. dy <= tower.range *. tower.range

let attack tower enemy =
  Enemy.take_damage enemy tower.damage
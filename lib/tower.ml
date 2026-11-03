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
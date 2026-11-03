type t = {
  position : Game_types.position;
  range : float;
  damage : int;
  cost : int;
}

val cost : int
val create : Game_types.position -> t
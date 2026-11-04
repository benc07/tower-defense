type t = {
  position : Game_types.position;
  range : float;
  damage : int;
  cost : int;
}

val cost : int
val create : Game_types.position -> t
val in_range : t -> Game_types.position -> bool
val attack : t -> Enemy.t -> Enemy.t
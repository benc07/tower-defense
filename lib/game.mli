(** Pure simulation. MS2 begins with one enemy on the spawn tile. *)
type t
val create : Map.t -> (t, string) result
val place_tower : t -> Game_types.position -> (t, string) result
val next_tick : t -> t
val map : t -> Map.t
val path : t -> Game_types.position list
val enemies : t -> Enemy.t list
val towers : t -> Tower.t list
val gold : t -> int
val lives : t -> int
val tick : t -> int
val lost : t -> bool
val enemy_position : t -> Enemy.t -> Game_types.position

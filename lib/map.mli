type t
(** Immutable map of terrain; tower occupancy belongs to Game. *)

val of_rows : Game_types.tile list list -> (t, string) result
val width : t -> int
val height : t -> int
val get_tile : t -> Game_types.position -> Game_types.tile option
val in_bounds : t -> Game_types.position -> bool
val can_place_tower : t -> Game_types.position -> bool
val start_position : t -> Game_types.position
val base_position : t -> Game_types.position

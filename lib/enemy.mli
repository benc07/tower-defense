type t = {
  id : int;
  health : int;
  path_index : int;
}
(** One MS2 enemy type; path_index is zero-based. *)

val create : int -> t
val move : t -> path_length:int -> t
val take_damage : t -> int -> t
val is_dead : t -> bool
val reached_base : t -> path_length:int -> bool
val reward : int

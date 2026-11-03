(** Shared coordinates and terrain. Coordinates use x increasing right and y increasing down. *)
type position = { x : int; y : int }
type tile = Empty | Path | Start | Base | Buildable
val equal_position : position -> position -> bool

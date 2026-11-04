(** Terminal-only commands and rendering; no game rules live here. *)
type command =
  | Show
  | Place of Game_types.position
  | Next
  | Help
  | Quit

val parse_command : string -> (command, string) result
val render : Game.t -> string
val run : Game.t -> unit

val parse_string : string -> (Map.t, string) result
(** MS2 map format: . empty, # path, S spawn, B base, T buildable. *)

val load : string -> (Map.t, string) result

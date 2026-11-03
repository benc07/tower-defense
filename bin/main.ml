
let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline
      "Usage: dune exec ./bin/main.exe -- maps/demo.map";
    exit 1
  );

  match Parser.load Sys.argv.(1) with
  | Error message ->
      prerr_endline ("Map error: " ^ message);
      exit 1

  | Ok map ->
      match Game.create map with
      | Error message ->
          prerr_endline ("Game error: " ^ message);
          exit 1

      | Ok game ->
          Gui.run game

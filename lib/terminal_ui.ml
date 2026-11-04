open Game_types

type command =
  | Show
  | Place of position
  | Next
  | Help
  | Quit

let parse_command input =
  match
    String.split_on_char ' ' (String.trim input)
    |> List.filter (fun word -> word <> "")
  with
  | [ "show" ] -> Ok Show
  | [ "next" ] -> Ok Next
  | [ "quit" ] -> Ok Quit
  | [ "help" ] -> Ok Help
  | [ "place"; x; y ] -> (
      match (int_of_string_opt x, int_of_string_opt y) with
      | Some x, Some y -> Ok (Place { x; y })
      | _ -> Error "Usage: place <integer-x> <integer-y>")
  | _ -> Error "Unknown command. Type help for available commands."

let render g =
  let map = Game.map g in
  let rows =
    Array.init (Map.height map) (fun y ->
        Array.init (Map.width map) (fun x ->
            match Map.get_tile map { x; y } with
            | Some Empty -> '.'
            | Some Path -> '#'
            | Some Start -> 'S'
            | Some Base -> 'B'
            | Some Buildable -> 'T'
            | None -> '?'))
  in
  List.iter
    (fun tower ->
      let p = tower.Tower.position in
      rows.(p.y).(p.x) <- 't')
    (Game.towers g);
  List.iter
    (fun enemy ->
      let p = Game.enemy_position g enemy in
      rows.(p.y).(p.x) <- 'E')
    (Game.enemies g);
  let board =
    Array.to_list rows
    |> List.map (fun row -> String.init (Array.length row) (Array.get row))
    |> String.concat "\n"
  in
  Printf.sprintf "Gold: %d | Lives: %d | Tick: %d | Enemies: %d\n%s"
    (Game.gold g) (Game.lives g) (Game.tick g)
    (List.length (Game.enemies g))
    board

let help = "Commands: show | place x y | next | help | quit"

let rec run state =
  print_endline (render state);
  if Game.lost state then print_endline "Game over: no lives remaining."
  else (
    print_string "> ";
    flush stdout;
    match read_line () with
    | exception End_of_file -> print_endline "Goodbye."
    | input -> (
        match parse_command input with
        | Error message ->
            print_endline message;
            run state
        | Ok Quit -> print_endline "Goodbye."
        | Ok Help ->
            print_endline help;
            run state
        | Ok Show -> run state
        | Ok Next -> run (Game.next_tick state)
        | Ok (Place p) -> (
            match Game.place_tower state p with
            | Ok new_state ->
                print_endline "Tower placed.";
                run new_state
            | Error message ->
                print_endline message;
                run state)))

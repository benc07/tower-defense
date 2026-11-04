open OUnit2
open Game_types

let unwrap = function
  | Ok value -> value
  | Error msg -> assert_failure msg

let pos x y = { x; y }
let sample = "..........\nS######B..\n..........\n..TT..TT..\n..........\n"

let game () =
  let map = unwrap (Parser.parse_string sample) in
  unwrap (Game.create map)

let is_error = function
  | Error _ -> true
  | Ok _ -> false

(* Command parser tests *)

let test_show_command _ =
  assert_equal (Ok Terminal_ui.Show) (Terminal_ui.parse_command "show")

let test_next_command _ =
  assert_equal (Ok Terminal_ui.Next) (Terminal_ui.parse_command "next")

let test_quit_command _ =
  assert_equal (Ok Terminal_ui.Quit) (Terminal_ui.parse_command "quit")

let test_help_command _ =
  assert_equal (Ok Terminal_ui.Help) (Terminal_ui.parse_command "help")

let test_place_command _ =
  assert_equal
    (Ok (Terminal_ui.Place (pos 3 3)))
    (Terminal_ui.parse_command "place 3 3")

let test_command_whitespace _ =
  assert_equal
    (Ok (Terminal_ui.Place (pos 3 3)))
    (Terminal_ui.parse_command "  place   3   3  ")

let test_invalid_coordinates _ =
  assert_bool "noninteger x" (is_error (Terminal_ui.parse_command "place x 3"));

  assert_bool "noninteger y" (is_error (Terminal_ui.parse_command "place 3 y"))

let test_missing_arguments _ =
  assert_bool "missing y coordinate"
    (is_error (Terminal_ui.parse_command "place 3"));

  assert_bool "no coordinates" (is_error (Terminal_ui.parse_command "place"))

let test_unknown_command _ =
  assert_bool "unknown command rejected"
    (is_error (Terminal_ui.parse_command "attack"))

let test_empty_command _ =
  assert_bool "empty command rejected" (is_error (Terminal_ui.parse_command ""))

(* Renderer tests *)

let test_initial_render _ =
  let output = Terminal_ui.render (game ()) in
  let lines = String.split_on_char '\n' output in

  assert_equal 6 (List.length lines);
  assert_equal "E######B.." (List.nth lines 2);
  assert_equal "..TT..TT.." (List.nth lines 4)

let test_tower_render _ =
  let g = game () in

  let g = unwrap (Game.place_tower g (pos 3 3)) in

  let output = Terminal_ui.render g in
  let lines = String.split_on_char '\n' output in

  assert_equal "..Tt..TT.." (List.nth lines 4)

let test_movement_render _ =
  let g = Game.next_tick (game ()) in
  let output = Terminal_ui.render g in
  let lines = String.split_on_char '\n' output in

  assert_equal "SE#####B.." (List.nth lines 2)

let test_render_is_pure _ =
  let g = game () in

  let first = Terminal_ui.render g in
  let second = Terminal_ui.render g in

  assert_equal first second;
  assert_equal 0 (Game.tick g);
  assert_equal 100 (Game.gold g)

let suite =
  "Terminal UI Tests"
  >::: [
         "show command" >:: test_show_command;
         "next command" >:: test_next_command;
         "quit command" >:: test_quit_command;
         "help command" >:: test_help_command;
         "place command" >:: test_place_command;
         "command whitespace" >:: test_command_whitespace;
         "invalid coordinates" >:: test_invalid_coordinates;
         "missing arguments" >:: test_missing_arguments;
         "unknown command" >:: test_unknown_command;
         "empty command" >:: test_empty_command;
         "initial render" >:: test_initial_render;
         "tower render" >:: test_tower_render;
         "movement render" >:: test_movement_render;
         "pure rendering" >:: test_render_is_pure;
       ]

let () = run_test_tt_main suite

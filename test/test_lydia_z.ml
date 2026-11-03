open OUnit2
open Game_types

let unwrap = function
  | Ok x -> x
  | Error msg -> assert_failure msg

let sample = "..........\nS######B..\n..........\n..TT..TT..\n..........\n"
let board () = unwrap (Parser.parse_string sample)
let game () = unwrap (Game.create (board ()))
let pos x y = { x; y }

let test_parser _ =
  let m = board () in
  assert_equal 10 (Map.width m);
  assert_equal 5 (Map.height m);
  assert_equal (Some Buildable) (Map.get_tile m (pos 3 3));
  assert_bool "bad character" (Result.is_error (Parser.parse_string "SXB"));
  assert_bool "unequal rows" (Result.is_error (Parser.parse_string "S#B\n.."));
  assert_bool "missing base" (Result.is_error (Parser.parse_string "S#"));
  assert_bool "blocked path" (Result.is_error (Parser.parse_string "S.B"));
  assert_bool "missing file"
    (Result.is_error (Parser.load "this-file-does-not-exist.map"))

let test_pathfinding _ =
  let path = Option.get (Pathfinding.find_path (board ())) in
  assert_equal 8 (List.length path);
  assert_equal (pos 0 1) (List.hd path);
  assert_equal (pos 7 1) (List.hd (List.rev path));
  let turning = unwrap (Parser.parse_string "S#..\n.#..\n.##B") in
  assert_equal 6 (List.length (Option.get (Pathfinding.find_path turning)))

let test_enemy _ =
  let e = Enemy.create 1 in
  assert_equal 30 e.Enemy.health;
  assert_equal 1 (Enemy.move e ~path_length:8).Enemy.path_index;
  assert_equal 20 (Enemy.take_damage e 10).Enemy.health;
  assert_bool "enemy dead" (Enemy.is_dead (Enemy.take_damage e 30));
  assert_bool "at base"
    (Enemy.reached_base { e with path_index = 7 } ~path_length:8)

let () =
  run_test_tt_main
    ("lydia_z"
    >::: [
           "parser" >:: test_parser;
           "pathfinding" >:: test_pathfinding;
           "enemy" >:: test_enemy;
         ])

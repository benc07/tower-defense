open OUnit2
open Game_types

let unwrap = function
  | Ok value -> value
  | Error msg -> assert_failure msg

let pos x y = { x; y }
let sample = "..........\nS######B..\n..........\n..TT..TT..\n..........\n"
let board () = unwrap (Parser.parse_string sample)

let is_error = function
  | Error _ -> true
  | Ok _ -> false

(* Map and parser tests *)

let test_valid_map _ =
  let map = board () in

  assert_equal 10 (Map.width map);
  assert_equal 5 (Map.height map);
  assert_equal (pos 0 1) (Map.start_position map);
  assert_equal (pos 7 1) (Map.base_position map)

let test_tile_types _ =
  let map = board () in

  assert_equal (Some Empty) (Map.get_tile map (pos 0 0));

  assert_equal (Some Start) (Map.get_tile map (pos 0 1));

  assert_equal (Some Path) (Map.get_tile map (pos 1 1));

  assert_equal (Some Base) (Map.get_tile map (pos 7 1));

  assert_equal (Some Buildable) (Map.get_tile map (pos 3 3))

let test_out_of_bounds _ =
  let map = board () in

  assert_equal None (Map.get_tile map (pos (-1) 0));
  assert_equal None (Map.get_tile map (pos 10 0));
  assert_equal None (Map.get_tile map (pos 0 5));

  assert_bool "negative coordinate" (not (Map.in_bounds map (pos (-1) 0)));

  assert_bool "valid coordinate" (Map.in_bounds map (pos 3 3))

let test_buildable_tiles _ =
  let map = board () in

  assert_bool "tower tile is buildable" (Map.can_place_tower map (pos 3 3));

  assert_bool "path cannot hold tower" (not (Map.can_place_tower map (pos 3 1)));

  assert_bool "empty terrain cannot hold tower"
    (not (Map.can_place_tower map (pos 0 0)))

let test_invalid_character _ =
  assert_bool "invalid character rejected"
    (is_error (Parser.parse_string "SXB"))

let test_invalid_dimensions _ =
  assert_bool "unequal row lengths" (is_error (Parser.parse_string "S#B\n.."));

  assert_bool "empty file" (is_error (Parser.parse_string ""))

let test_missing_start _ =
  assert_bool "missing spawn" (is_error (Parser.parse_string "##B"))

let test_missing_base _ =
  assert_bool "missing base" (is_error (Parser.parse_string "S##"))

let test_duplicate_start _ =
  assert_bool "multiple spawns" (is_error (Parser.parse_string "SS#B"))

let test_duplicate_base _ =
  assert_bool "multiple bases" (is_error (Parser.parse_string "S#BB"))

let test_disconnected_path _ =
  assert_bool "unreachable base" (is_error (Parser.parse_string "S.B"))

let test_missing_file _ =
  assert_bool "missing file returns error"
    (is_error (Parser.load "nonexistent_ms2_test_file.map"))

(* Pathfinding tests *)

let test_straight_path _ =
  let path = Option.get (Pathfinding.find_path (board ())) in

  assert_equal 8 (List.length path);
  assert_equal (pos 0 1) (List.hd path);
  assert_equal (pos 7 1) (List.hd (List.rev path))

let test_turning_path _ =
  let map = unwrap (Parser.parse_string "S#..\n.#..\n.##B") in

  let path = Option.get (Pathfinding.find_path map) in

  let expected = [ pos 0 0; pos 1 0; pos 1 1; pos 1 2; pos 2 2; pos 3 2 ] in

  assert_equal expected path

let test_shortest_path _ =
  (* Several routes exist through this grid. Manhattan distance from (0,0) to
     (2,2) is 4 edges, so the shortest path has exactly 5 positions. *)
  let map = unwrap (Parser.parse_string "S##\n###\n##B") in

  let path = Option.get (Pathfinding.find_path map) in

  assert_equal 5 (List.length path);
  assert_equal (pos 0 0) (List.hd path);
  assert_equal (pos 2 2) (List.hd (List.rev path));

  let rec check_adjacent = function
    | a :: (b :: _ as rest) ->
        let distance = abs (a.x - b.x) + abs (a.y - b.y) in

        assert_equal 1 distance;
        check_adjacent rest
    | _ -> ()
  in

  check_adjacent path

let test_unreachable_path _ =
  let map = unwrap (Map.of_rows [ [ Start; Empty; Base ] ]) in

  assert_equal None (Pathfinding.find_path map)

(* Enemy tests *)

let test_enemy_initialization _ =
  let enemy = Enemy.create 42 in

  assert_equal 42 enemy.Enemy.id;
  assert_equal 30 enemy.Enemy.health;
  assert_equal 0 enemy.Enemy.path_index

let test_enemy_movement _ =
  let enemy = Enemy.create 1 in
  let moved = Enemy.move enemy ~path_length:8 in

  assert_equal 1 moved.Enemy.path_index;
  assert_equal 0 enemy.Enemy.path_index

let test_enemy_stops_at_end _ =
  let enemy = { (Enemy.create 1) with path_index = 7 } in

  let moved = Enemy.move enemy ~path_length:8 in

  assert_equal 7 moved.Enemy.path_index

let test_enemy_damage _ =
  let enemy = Enemy.create 1 in
  let damaged = Enemy.take_damage enemy 10 in

  assert_equal 20 damaged.Enemy.health;
  assert_equal 30 enemy.Enemy.health

let test_enemy_death _ =
  let enemy = Enemy.take_damage (Enemy.create 1) 30 in

  assert_bool "enemy should be dead" (Enemy.is_dead enemy);

  assert_equal 0 enemy.Enemy.health

let test_overkill_damage _ =
  let enemy = Enemy.take_damage (Enemy.create 1) 100 in

  assert_equal 0 enemy.Enemy.health;
  assert_bool "enemy is dead" (Enemy.is_dead enemy)

let test_negative_damage _ =
  let enemy = Enemy.take_damage (Enemy.create 1) (-10) in

  assert_equal 30 enemy.Enemy.health

let test_reached_base _ =
  let enemy = { (Enemy.create 1) with path_index = 7 } in

  assert_bool "enemy reaches base" (Enemy.reached_base enemy ~path_length:8);

  let earlier = { enemy with path_index = 6 } in

  assert_bool "enemy has not reached base"
    (not (Enemy.reached_base earlier ~path_length:8))

let suite =
  "Map, Parser, Pathfinding, Enemy Tests"
  >::: [
         "valid map" >:: test_valid_map;
         "tile types" >:: test_tile_types;
         "out of bounds" >:: test_out_of_bounds;
         "buildable tiles" >:: test_buildable_tiles;
         "invalid character" >:: test_invalid_character;
         "invalid dimensions" >:: test_invalid_dimensions;
         "missing start" >:: test_missing_start;
         "missing base" >:: test_missing_base;
         "duplicate start" >:: test_duplicate_start;
         "duplicate base" >:: test_duplicate_base;
         "disconnected path" >:: test_disconnected_path;
         "missing file" >:: test_missing_file;
         "straight path" >:: test_straight_path;
         "turning path" >:: test_turning_path;
         "shortest path" >:: test_shortest_path;
         "unreachable path" >:: test_unreachable_path;
         "enemy initialization" >:: test_enemy_initialization;
         "enemy movement" >:: test_enemy_movement;
         "enemy stops at end" >:: test_enemy_stops_at_end;
         "enemy damage" >:: test_enemy_damage;
         "enemy death" >:: test_enemy_death;
         "overkill damage" >:: test_overkill_damage;
         "negative damage" >:: test_negative_damage;
         "reached base" >:: test_reached_base;
       ]

let () = run_test_tt_main suite

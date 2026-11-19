
open OUnit2
open Game_types

let unwrap = function
  | Ok value -> value
  | Error msg -> assert_failure msg

let pos x y = { x; y }

let sample =
  "..........\n\
   S######B..\n\
   ..........\n\
   ..TT..TT..\n\
   ..........\n"

let game () =
  let map = unwrap (Parser.parse_string sample) in
  unwrap (Game.create map)

let advance n initial =
  let rec loop remaining state =
    if remaining = 0 then state
    else loop (remaining - 1) (Game.next_tick state)
  in
  loop n initial

let place state x y =
  unwrap (Game.place_tower state (pos x y))

(* Initial game state *)

let test_initial_state _ =
  let g = game () in

  assert_equal 100 (Game.gold g);
  assert_equal 5 (Game.lives g);
  assert_equal 0 (Game.tick g);
  assert_equal 1 (List.length (Game.enemies g));
  assert_equal 0 (List.length (Game.towers g));
  assert_bool "game should not be lost"
    (not (Game.lost g))

let test_initial_enemy _ =
  let g = game () in
  let enemy = List.hd (Game.enemies g) in

  assert_equal 30 enemy.Enemy.health;
  assert_equal 0 enemy.Enemy.path_index;
  assert_equal (pos 0 1)
    (Game.enemy_position g enemy)

(* Tower placement and economy *)

let test_valid_placement _ =
  let g = place (game ()) 3 3 in

  assert_equal 80 (Game.gold g);
  assert_equal 1 (List.length (Game.towers g));

  let tower = List.hd (Game.towers g) in
  assert_equal (pos 3 3) tower.Tower.position

let test_multiple_placements _ =
  let g = game () in
  let g = place g 2 3 in
  let g = place g 3 3 in
  let g = place g 6 3 in

  assert_equal 40 (Game.gold g);
  assert_equal 3 (List.length (Game.towers g))

let test_invalid_placement _ =
  let g = game () in

  assert_bool "cannot place on path"
    (Result.is_error
       (Game.place_tower g (pos 2 1)));

  assert_bool "cannot place on spawn"
    (Result.is_error
       (Game.place_tower g (pos 0 1)));

  assert_bool "cannot place on base"
    (Result.is_error
       (Game.place_tower g (pos 7 1)));

  assert_bool "cannot place on empty tile"
    (Result.is_error
       (Game.place_tower g (pos 0 0)));

  assert_bool "cannot place outside map"
    (Result.is_error
       (Game.place_tower g (pos 20 20)))

let test_occupied_tile _ =
  let g = place (game ()) 3 3 in

  assert_bool "duplicate placement rejected"
    (Result.is_error
       (Game.place_tower g (pos 3 3)));

  assert_equal 80 (Game.gold g);
  assert_equal 1 (List.length (Game.towers g))

let test_insufficient_gold _ =
  (* Six buildable tiles allow us to exhaust
     the starting 100 gold. *)
  let map =
    unwrap
      (Parser.parse_string
         "S######B..\n\
          TTTTTT....\n")
  in

  let g = unwrap (Game.create map) in

  let g =
    List.fold_left
      (fun state x -> place state x 1)
      g [0; 1; 2; 3; 4]
  in

  assert_equal 0 (Game.gold g);
  assert_equal 5 (List.length (Game.towers g));

  assert_bool "insufficient gold rejected"
    (Result.is_error
       (Game.place_tower g (pos 5 1)));

  assert_equal 0 (Game.gold g);
  assert_equal 5 (List.length (Game.towers g))

let test_original_state_unchanged _ =
  let original = game () in
  let updated = place original 3 3 in

  assert_equal 100 (Game.gold original);
  assert_equal 0 (List.length (Game.towers original));

  assert_equal 80 (Game.gold updated);
  assert_equal 1 (List.length (Game.towers updated))

(* Tick engine *)

let test_tick_progression _ =
  let g = game () in
  let g1 = Game.next_tick g in
  let g2 = Game.next_tick g1 in

  assert_equal 0 (Game.tick g);
  assert_equal 1 (Game.tick g1);
  assert_equal 2 (Game.tick g2)

let test_enemy_movement _ =
  let g = game () in

  let g1 = advance 1 g in
  let g2 = advance 2 g in
  let g3 = advance 3 g in

  let position state =
    Game.enemy_position state
      (List.hd (Game.enemies state))
  in

  assert_equal (pos 1 1) (position g1);
  assert_equal (pos 2 1) (position g2);
  assert_equal (pos 3 1) (position g3)

(* Combat integration *)

let test_tower_attacks_in_range _ =
  let g = place (game ()) 3 3 in

  let g1 = advance 1 g in
  let g2 = advance 2 g in

  assert_equal 30
    (List.hd (Game.enemies g1)).Enemy.health;

  assert_equal 20
    (List.hd (Game.enemies g2)).Enemy.health

let test_enemy_death _ =
  let g = place (game ()) 3 3 in

  let g3 = advance 3 g in
  let g4 = advance 4 g in

  assert_equal 1 (List.length (Game.enemies g3));
  assert_equal 10
    (List.hd (Game.enemies g3)).Enemy.health;

  assert_equal [] (Game.enemies g4)

let test_gold_reward _ =
  let g = place (game ()) 3 3 in
  let g = advance 4 g in

  (* 100 starting gold - 20 tower cost
     + 10 reward = 90. *)
  assert_equal 90 (Game.gold g);
  assert_equal 5 (Game.lives g)

let test_reward_only_once _ =
  let g = place (game ()) 3 3 in

  let g4 = advance 4 g in
  let g8 = advance 8 g in

  assert_equal 90 (Game.gold g4);
  assert_equal 90 (Game.gold g8);
  assert_equal [] (Game.enemies g8)

(* Enemy reaching the base *)

let test_base_reached _ =
  let g = advance 7 (game ()) in

  assert_equal [] (Game.enemies g);
  assert_equal 4 (Game.lives g);
  assert_equal 100 (Game.gold g);
  assert_equal 7 (Game.tick g)

let test_base_not_reached_early _ =
  let g = advance 6 (game ()) in

  assert_equal 5 (Game.lives g);
  assert_equal 1 (List.length (Game.enemies g))

let test_life_loss_only_once _ =
  let g7 = advance 7 (game ()) in
  let g10 = advance 10 (game ()) in

  assert_equal 4 (Game.lives g7);
  assert_equal 4 (Game.lives g10)

(* Game creation errors *)

let test_disconnected_map _ =
  let map =
    unwrap
      (Map.of_rows
         [[Start; Empty; Base]])
  in

  assert_bool "unreachable base rejected"
    (Result.is_error (Game.create map))

let suite =
  "Game Engine Tests" >::: [
    "initial state" >:: test_initial_state;
    "initial enemy" >:: test_initial_enemy;
    "valid placement" >:: test_valid_placement;
    "multiple placements" >:: test_multiple_placements;
    "invalid placement" >:: test_invalid_placement;
    "occupied tile" >:: test_occupied_tile;
    "insufficient gold" >:: test_insufficient_gold;
    "immutable state" >:: test_original_state_unchanged;
    "tick progression" >:: test_tick_progression;
    "enemy movement" >:: test_enemy_movement;
    "attack in range" >:: test_tower_attacks_in_range;
    "enemy death" >:: test_enemy_death;
    "gold reward" >:: test_gold_reward;
    "reward only once" >:: test_reward_only_once;
    "base reached" >:: test_base_reached;
    "base not reached early" >:: test_base_not_reached_early;
    "life loss only once" >:: test_life_loss_only_once;
    "disconnected map" >:: test_disconnected_map;
  ]

let () = run_test_tt_main suite

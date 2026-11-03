open OUnit2
open Game_types
let unwrap = function Ok x -> x | Error msg -> assert_failure msg
let sample = "..........\nS######B..\n..........\n..TT..TT..\n..........\n"
let board () = unwrap (Parser.parse_string sample)
let game () = unwrap (Game.create (board ()))
let pos x y = {x;y}
let test_place _ =
  let g = game () in
  assert_bool "reject path" (Result.is_error (Game.place_tower g (pos 0 1)));
  let g = unwrap (Game.place_tower g (pos 3 3)) in
  assert_equal 80 (Game.gold g);
  assert_equal 1 (List.length (Game.towers g));
  assert_bool "occupied tile" (Result.is_error (Game.place_tower g (pos 3 3)))
let test_combat _ =
  let g = unwrap (Game.place_tower (game ()) (pos 3 3)) in
  let g1 = Game.next_tick g in
  assert_equal 1 (Game.tick g1);
  assert_equal 30 (List.hd (Game.enemies g1)).Enemy.health;
  let g2 = Game.next_tick g1 in
  assert_equal 20 (List.hd (Game.enemies g2)).Enemy.health;
  let g3 = Game.next_tick g2 in
  assert_equal 10 (List.hd (Game.enemies g3)).Enemy.health;
  let g4 = Game.next_tick g3 in
  assert_equal [] (Game.enemies g4);
  assert_equal 90 (Game.gold g4);
  assert_equal 5 (Game.lives g4)
let test_base _ =
  let rec advance n g = if n = 0 then g else advance (n-1) (Game.next_tick g) in
  let g = advance 7 (game ()) in
  assert_equal [] (Game.enemies g);
  assert_equal 4 (Game.lives g);
  assert_equal 100 (Game.gold g)
let () = run_test_tt_main ("pranav" >::: [
  "place" >:: test_place;
  "combat" >:: test_combat;
  "base" >:: test_base;
])

open OUnit2
open Game_types

let pos x y = { x; y }

let test_tower _ =
  let t = Tower.create (pos 0 0) in
  assert_bool "inside range" (Tower.in_range t (pos 1 2));
  assert_bool "outside range" (not (Tower.in_range t (pos 2 2)));
  assert_equal 20 (Tower.attack t (Enemy.create 0)).Enemy.health

let () =
  run_test_tt_main
    ("ben" >::: [
      "tower" >:: test_tower;
    ])
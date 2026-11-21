open OUnit2
open Game_types

let pos x y = { x; y }

let test_tower_initialization _ =
  let tower = Tower.create (pos 3 3) in

  assert_equal (pos 3 3) tower.Tower.position;
  assert_equal 2.5 tower.Tower.range;
  assert_equal 10 tower.Tower.damage;
  assert_equal 20 tower.Tower.cost;
  assert_equal 20 Tower.cost

let test_enemy_at_tower _ =
  let tower = Tower.create (pos 3 3) in

  assert_bool "same tile is in range"
    (Tower.in_range tower (pos 3 3))

let test_inside_range _ =
  let tower = Tower.create (pos 3 3) in

  (* Distance = sqrt(1^2 + 2^2) = sqrt(5). *)
  assert_bool "enemy within range"
    (Tower.in_range tower (pos 4 5))

let test_outside_range _ =
  let tower = Tower.create (pos 3 3) in

  (* Distance = sqrt(2^2 + 2^2) = sqrt(8),
     which exceeds the tower's 2.5 range. *)
  assert_bool "enemy outside range"
    (not (Tower.in_range tower (pos 5 5)))

let test_exact_range_boundary _ =
  (* Use a custom tower range of 5 to test
     the 3-4-5 right triangle exactly. *)
  let tower =
    { (Tower.create (pos 0 0)) with
      range = 5.0 }
  in

  assert_bool "distance 5 is in range"
    (Tower.in_range tower (pos 3 4))

let test_just_outside_boundary _ =
  let tower =
    { (Tower.create (pos 0 0)) with
      range = 5.0 }
  in

  (* Distance = sqrt(26), greater than 5. *)
  assert_bool "distance greater than 5"
    (not (Tower.in_range tower (pos 1 5)))

let test_range_symmetry _ =
  let tower1 = Tower.create (pos 1 2) in
  let tower2 = Tower.create (pos 3 4) in

  assert_equal
    (Tower.in_range tower1 (pos 3 4))
    (Tower.in_range tower2 (pos 1 2))

let test_attack_damage _ =
  let tower = Tower.create (pos 3 3) in
  let enemy = Enemy.create 1 in

  let damaged = Tower.attack tower enemy in

  assert_equal 20 damaged.Enemy.health;
  assert_equal 30 enemy.Enemy.health

let test_repeated_attacks _ =
  let tower = Tower.create (pos 3 3) in
  let enemy = Enemy.create 1 in

  let after_one = Tower.attack tower enemy in
  let after_two = Tower.attack tower after_one in
  let after_three = Tower.attack tower after_two in

  assert_equal 20 after_one.Enemy.health;
  assert_equal 10 after_two.Enemy.health;
  assert_equal 0 after_three.Enemy.health;

  assert_bool "enemy dies after three attacks"
    (Enemy.is_dead after_three)

let test_attack_overkill _ =
  let tower = Tower.create (pos 3 3) in

  let enemy =
    Enemy.take_damage (Enemy.create 1) 25
  in

  let result = Tower.attack tower enemy in

  assert_equal 0 result.Enemy.health;
  assert_bool "enemy dead" (Enemy.is_dead result)

let suite =
  "Tower and Combat Tests" >::: [
    "tower initialization" >:: test_tower_initialization;
    "enemy at tower" >:: test_enemy_at_tower;
    "inside range" >:: test_inside_range;
    "outside range" >:: test_outside_range;
    "exact boundary" >:: test_exact_range_boundary;
    "outside boundary" >:: test_just_outside_boundary;
    "range symmetry" >:: test_range_symmetry;
    "attack damage" >:: test_attack_damage;
    "repeated attacks" >:: test_repeated_attacks;
    "attack overkill" >:: test_attack_overkill;
  ]

let () = run_test_tt_main suite

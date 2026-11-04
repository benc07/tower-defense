open Graphics
open Game_types

let window_w = 1120
let window_h = 700
let board_x = 70
let board_top = 525
let sidebar_x = 810
let rgb = Graphics.rgb

(* Color palette *)
let background = rgb 13 20 34
let panel = rgb 23 34 53
let panel_light = rgb 35 49 72
let white = rgb 238 244 252
let muted = rgb 151 167 190
let teal = rgb 58 211 190
let gold_color = rgb 255 196 87
let danger = rgb 243 109 113
let road = rgb 79 95 118
let grass = rgb 34 70 70
let build = rgb 38 102 99

(* Drawing helpers *)
let rect c x y w h =
  set_color c;
  fill_rect x y w h

let circle c x y r =
  set_color c;
  fill_circle x y r

let outline c x y w h =
  set_color c;
  draw_rect x y w h

let label ?(size = 16) c x y s =
  set_color c;
  set_text_size size;
  moveto x y;
  draw_string s

let center_label ?(size = 16) c x y s =
  set_text_size size;
  let w, _ = text_size s in
  label ~size c (x - (w / 2)) y s

(* Clickable buttons *)
type button = {
  x : int;
  y : int;
  w : int;
  h : int;
}

let contains (b : button) x y =
  x >= b.x && x < b.x + b.w && y >= b.y && y < b.y + b.h

let next_button = { x = 835; y = 301; w = 250; h = 56 }
let auto_button = { x = 835; y = 232; w = 250; h = 54 }
let reset_button = { x = 835; y = 164; w = 120; h = 52 }
let quit_button = { x = 965; y = 164; w = 120; h = 52 }

let draw_button (b : button) color text =
  rect color b.x b.y b.w b.h;
  center_label ~size:18 white (b.x + (b.w / 2)) (b.y + (b.h / 2) - 6) text

(* Map coordinate conversions *)
let tile_size map = min 66 (min (680 / Map.width map) (440 / Map.height map))

let tile_rect map (p : position) =
  let cell = tile_size map in
  (board_x + (p.x * cell), board_top - ((p.y + 1) * cell), cell)

let tile_at map x y =
  let cell = tile_size map in
  let col = (x - board_x) / cell in
  let row = (board_top - y) / cell in

  if
    x < board_x || y >= board_top || col < 0 || row < 0
    || col >= Map.width map
    || row >= Map.height map
  then None
  else Some ({ x = col; y = row } : position)

(* Individual terrain tiles *)
let paint_tile g (hover : position option) x y =
  let map = Game.map g in
  let p : position = { x; y } in
  let px, py, cell = tile_rect map p in

  let terrain =
    match Map.get_tile map p with
    | Some Empty -> grass
    | Some Path -> road
    | Some Start -> rgb 47 119 109
    | Some Base -> rgb 136 73 85
    | Some Buildable -> build
    | None -> background
  in

  rect terrain (px + 2) (py + 2) (cell - 4) (cell - 4);

  (match Map.get_tile map p with
  | Some Buildable ->
      outline (rgb 85 163 155) (px + 7) (py + 7) (cell - 14) (cell - 14);

      center_label ~size:18 teal (px + (cell / 2)) (py + (cell / 2) - 6) "+"
  | Some Start ->
      center_label ~size:15 white (px + (cell / 2)) (py + (cell / 2) - 6) "S"
  | Some Base ->
      center_label ~size:15 white (px + (cell / 2)) (py + (cell / 2) - 6) "B"
  | _ -> ());

  if hover = Some p && Map.can_place_tower map p then
    outline gold_color (px + 2) (py + 2) (cell - 4) (cell - 4)

(* Tower drawing *)
let draw_tower map tower =
  let px, py, cell = tile_rect map tower.Tower.position in

  let cx = px + (cell / 2) in
  let cy = py + (cell / 2) in

  circle (rgb 14 38 52) cx cy (cell / 3);

  circle teal cx cy (cell / 4);

  circle white cx cy (cell / 10);

  set_color (rgb 95 235 211);
  set_line_width 3;
  draw_circle cx cy (cell / 3);
  set_line_width 1

(* Enemy drawing, including health *)
let draw_enemy g enemy =
  let map = Game.map g in
  let p : position = Game.enemy_position g enemy in
  let px, py, cell = tile_rect map p in

  let cx = px + (cell / 2) in
  let cy = py + (cell / 2) in

  circle (rgb 55 28 42) cx cy (cell / 3);

  circle danger cx cy (cell / 4);

  let health = max 0 enemy.Enemy.health in
  let max_health = 30 in
  let bar_w = cell - 18 in

  rect (rgb 30 39 52) (px + 9) (py + cell - 12) bar_w 6;

  rect teal (px + 9) (py + cell - 12) (bar_w * health / max_health) 6

(* Draw the complete board *)
let draw_board g (hover : position option) =
  let map = Game.map g in
  let cell = tile_size map in

  let width = Map.width map * cell in
  let height = Map.height map * cell in

  rect panel (board_x - 18) (board_top - height - 24) (width + 36) (height + 50);

  for y = 0 to Map.height map - 1 do
    for x = 0 to Map.width map - 1 do
      paint_tile g hover x y
    done
  done;

  List.iter (draw_tower map) (Game.towers g);

  List.iter (draw_enemy g) (Game.enemies g);

  (* Coordinate labels *)
  for x = 0 to Map.width map - 1 do
    center_label ~size:13 muted
      (board_x + (x * cell) + (cell / 2))
      (board_top + 10) (string_of_int x)
  done;

  for y = 0 to Map.height map - 1 do
    center_label ~size:13 muted (board_x - 34)
      (board_top - (y * cell) - (cell / 2) - 5)
      (string_of_int y)
  done

(* Sidebar and status information *)
let draw_sidebar g autoplay message =
  rect panel sidebar_x 72 290 540;

  label ~size:21 white 835 561 "GAME STATUS";

  rect panel_light 833 466 254 75;

  label ~size:14 muted 846 518 "GOLD";

  label ~size:22 gold_color 846 486 (string_of_int (Game.gold g));

  label ~size:14 muted 970 518 "LIVES";

  label ~size:22 danger 970 486 (string_of_int (Game.lives g));

  label ~size:15 muted 836 431 ("TICK   " ^ string_of_int (Game.tick g));

  label ~size:15 muted 836 405
    ("ENEMIES   " ^ string_of_int (List.length (Game.enemies g)));

  draw_button next_button teal "NEXT TICK";

  draw_button auto_button
    (if autoplay then danger else panel_light)
    (if autoplay then "PAUSE" else "AUTO PLAY");

  draw_button reset_button panel_light "RESTART";

  draw_button quit_button panel_light "QUIT";

  label ~size:13 muted 836 125 "CLICK A + TILE TO BUILD";

  label ~size:13 muted 836 102 ("TOWER COST: " ^ string_of_int Tower.cost);

  (* Status message at bottom *)
  rect panel 52 18 1048 43;

  label ~size:15 (if Game.lost g then danger else white) 70 32 message

let draw g autoplay (hover : position option) message =
  rect background 0 0 window_w window_h;

  label ~size:30 white 54 645 "TOWER DEFENSE";

  label ~size:16 muted 57 611 "MS2  /  Tactical Grid Defense";

  label ~size:16 muted 70 549 "DEFEND THE BASE";

  draw_board g hover;
  draw_sidebar g autoplay message;

  synchronize ()

(* Main graphical event loop *)
let run initial =
  open_graph (Printf.sprintf " %dx%d" window_w window_h);

  set_window_title "Tower Defense - MS2";
  auto_synchronize false;

  let current = ref initial in
  let autoplay = ref false in

  let message = ref "Click a + tile to build a tower. Then press NEXT TICK." in

  let previous_down = ref false in
  let last_step = ref (Unix.gettimeofday ()) in
  let running = ref true in

  (* Advance one simulation tick *)
  let advance () =
    let before = !current in
    current := Game.next_tick before;

    if Game.lost !current then (
      autoplay := false;
      message := "GAME OVER - no lives remaining. Click RESTART to try again.")
    else if Game.enemies before <> [] && Game.enemies !current = [] then (
      autoplay := false;

      if Game.gold !current > Game.gold before then
        message := "Enemy defeated! Gold awarded. Click RESTART to replay."
      else message := "Enemy reached the base! Click RESTART to replay.")
    else if Game.enemies !current = [] then (
      autoplay := false;
      message := "No active enemies. Click RESTART to replay.")
    else message := "Enemies move one tile per tick; towers attack in range."
  in

  (try
     while !running do
       let event = wait_next_event [ Poll ] in

       let hover = tile_at (Game.map !current) event.mouse_x event.mouse_y in

       (* Handle a new mouse click *)
       (if event.button && not !previous_down then
          let x = event.mouse_x in
          let y = event.mouse_y in

          if contains next_button x y then (
            autoplay := false;
            advance ())
          else if contains auto_button x y then
            if Game.enemies !current <> [] && not (Game.lost !current) then (
              autoplay := not !autoplay;
              last_step := Unix.gettimeofday ();

              message := if !autoplay then "Auto play running..." else "Paused.")
            else message := "No active enemies. Click RESTART to replay."
          else if contains reset_button x y then (
            current := initial;
            autoplay := false;
            message := "New game. Click a + tile to place a tower.")
          else if contains quit_button x y then running := false
          else
            match hover with
            | None -> ()
            | Some pos -> (
                match Game.place_tower !current pos with
                | Ok g ->
                    current := g;
                    message :=
                      Printf.sprintf "Tower built at (%d, %d). -%d gold." pos.x
                        pos.y Tower.cost
                | Error err -> message := err));

       previous_down := event.button;

       (* Optional keyboard shortcuts *)
       (if event.keypressed then
          match event.key with
          | 'q' | 'Q' -> running := false
          | ' ' | 'n' | 'N' ->
              autoplay := false;
              advance ()
          | 'r' | 'R' ->
              current := initial;
              autoplay := false
          | _ -> ());

       (* Automatic simulation playback *)
       let now = Unix.gettimeofday () in

       if !autoplay && now -. !last_step >= 0.7 then (
         last_step := now;
         advance ());

       if !running then draw !current !autoplay hover !message;

       Unix.sleepf 0.025
     done
   with Graphic_failure _ -> ());

  close_graph ()

open Game_types

let tile_of_char = function
  | '.' -> Some Empty
  | '#' -> Some Path
  | 'S' -> Some Start
  | 'B' -> Some Base
  | 'T' -> Some Buildable
  | _ -> None

let parse_string input =
  let lines = String.split_on_char '\n' input in
  let lines =
    match List.rev lines with
    | "" :: rest -> List.rev rest
    | _ -> lines
  in
  let rec parse_rows y acc = function
    | [] -> Map.of_rows (List.rev acc)
    | line :: rest -> (
        let rec parse_chars x acc =
          if x = String.length line then Ok (List.rev acc)
          else
            match tile_of_char line.[x] with
            | Some tile -> parse_chars (x + 1) (tile :: acc)
            | None ->
                Error
                  (Printf.sprintf "Invalid tile '%c' at line %d, column %d"
                     line.[x] (y + 1) (x + 1))
        in
        match parse_chars 0 [] with
        | Error _ as e -> e
        | Ok row -> parse_rows (y + 1) (row :: acc) rest)
  in
  match parse_rows 0 [] lines with
  | Error _ as e -> e
  | Ok map -> (
      match Pathfinding.find_path map with
      | None -> Error "No path from spawn (S) to base (B)"
      | Some _ -> Ok map)

let load filename =
  try
    let ic = open_in filename in
    let data =
      try
        let length = in_channel_length ic in
        let contents = really_input_string ic length in
        close_in ic;
        contents
      with e ->
        close_in_noerr ic;
        raise e
    in
    parse_string data
  with Sys_error msg -> Error ("Cannot read map: " ^ msg)

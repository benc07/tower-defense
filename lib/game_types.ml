type position = { x : int; y : int }
type tile = Empty | Path | Start | Base | Buildable
let equal_position a b = a.x = b.x && a.y = b.y

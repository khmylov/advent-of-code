open System.IO
open System.Text.RegularExpressions

type Content = | Sand | Clay | Water | Wet
type Grid = Content[,]
type Point = { X: int; Y: int } with
  member this.Down = {this with Y = this.Y + 1}
  member this.Left = {this with X = this.X - 1}
  member this.Right = {this with X = this.X + 1}

module Array2D =
  let toSeq (arr: 'T[,]): ('T * int * int) seq = seq {
    for y in 0..Array2D.length2 arr - 1 do
      for x in 0..Array2D.length1 arr - 1 do
        yield arr.[x, y], x, y
  }

let (|RegEx|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [for g in m.Groups -> g.Value]) else None

let parseInt = System.Int32.Parse

let parseCoord (input: string) =
  if not <| input.Contains(".") then match input with | RegEx "(.)=(\\d+)" [name; pos] -> (name, parseInt pos, parseInt pos)
  else match input with | RegEx "(.)=(\\d+)\\.\\.(\\d+)" [name; from'; to'] -> (name, parseInt from', parseInt to')

let parseLine (input: string) =
  input.Split([|','|], 2)
  |> Array.map (fun xs -> xs.Trim() |> parseCoord)
  |> fun xs -> match xs.[0] with | "x", _, _ -> xs.[0], xs.[1] | _ -> xs.[1], xs.[0]

let gridInput =
  Path.Combine (__SOURCE_DIRECTORY__, "day17-input.txt")
  |> File.ReadAllLines
  |> Array.map parseLine

let initialGrid =
  let inline extract a = gridInput |> Seq.map (a >> fun (_, _, x) -> x) |> Seq.max
  let maxX, maxY = extract fst, extract snd
  let grid = Array2D.create (maxX + 1) (maxY + 1) Sand

  for ((_, fromX, toX), (_, fromY, toY)) in gridInput do
    for x in fromX..toX do
      for y in fromY..toY do
        grid.[x, y] <- Clay
  grid

let isInGrid grid point =
  point.X >= 0 && point.X <= Array2D.length1 grid - 1 && point.Y >= 0 && point.Y <= Array2D.length2 grid - 1

let getCell grid point =
  if isInGrid grid point then Some(grid.[point.X, point.Y]) else None

let setCell (grid: Grid) point value =
  grid.[point.X, point.Y] <- value

let isStale grid point =
  match getCell grid point with | Some Water | Some Clay -> true | _ -> false

let isSand grid point =
  getCell grid point = Some Sand

let rec hasWallLeft grid point =
  isInGrid grid point && isStale grid point.Down && (getCell grid point.Left = Some Clay || hasWallLeft grid point.Left)

let rec hasWallRight grid point =
  isInGrid grid point && isStale grid point.Down && (getCell grid point.Right = Some Clay || hasWallRight grid point.Right)

// Mark this cell and all to the left as water until hit a wall
let rec fillLeft grid (point: Point) =
  let left = point.Left
  if getCell grid left <> Some Clay then
    setCell grid left Water
    fillLeft grid left

// Mark this cell and all to the right as water until hit a wall
let rec fillRight grid (point: Point) =
  let right = point.Right
  if getCell grid right <> Some Clay then
    setCell grid right Water
    fillRight grid right

let rec flowWater grid (point: Point) =
  let down, left, right = point.Down, point.Left, point.Right
  if isSand grid down then
    setCell grid down Wet
    flowWater grid down

  if isStale grid down && isSand grid left then
    setCell grid left Wet
    flowWater grid left

  if isStale grid down && isSand grid right then
    setCell grid right Wet
    flowWater grid right

  if isStale grid down && hasWallLeft grid point && hasWallRight grid point then
    fillLeft grid point
    fillRight grid point
    setCell grid point Water

let rec countWater grid filter =
  let ys = grid |> Array2D.toSeq |> Seq.filter (fun (c, _, _) -> c = Clay) |> Seq.map (fun (_, _, y) -> y)
  let minY, maxY = Seq.min ys, Seq.max ys
  grid |> Array2D.toSeq |> Seq.filter (fun (c, _, y) -> y >= minY && y <= maxY && filter c) |> Seq.length

flowWater initialGrid { X = 500; Y = 0 }
let part1 = countWater initialGrid (fun c -> c = Water || c = Wet)
let part2 = countWater initialGrid (fun c -> c = Water)

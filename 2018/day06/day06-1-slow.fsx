#time "on";;
open System
open System.Collections.Generic
open System.IO

type CoordId = int
type Coord = {Id: CoordId; X: int; Y: int}
let parse (id: CoordId) (str: string) =
  let s = str.Split ',' |> Array.map (Int32.Parse)
  {Id = id; X = s.[0]; Y = s.[1]}

let distance fromX fromY toX toY = abs (fromY - toY) + abs (fromX - toX)
let mapMax fn = Seq.map fn >> Seq.max
let inc = (+) 1

let buildGrid coords =
  let width = coords |> mapMax (fun x -> x.X) |> inc
  let height = coords |> mapMax (fun x -> x.Y) |> inc
  Array2D.init width height (fun x y ->
    let (_, minDistanceGroup) =
      coords
      |> Seq.groupBy (fun coord -> distance x y (coord.X) (coord.Y))
      |> Seq.minBy fst
    if (Seq.length minDistanceGroup <> 1) then []
    else minDistanceGroup |> Seq.map (fun x -> x.Id) |> List.ofSeq
  )

let buildPath fromX fromY maxX maxY = seq {
  let visited = HashSet<int*int>()
  let visit x y = let p = x, y in visited.Add p |> ignore; p
  let canVisit x y = x >= 0 && y >= 0 && x <= maxX && y <= maxY && not (visited.Contains ((x, y)))

  let stack = new Stack<int*int>()
  stack.Push (fromX, fromY)

  while stack.Count > 0 do
    let (x, y) = stack.Pop()
    if canVisit x y then
      let p = visit x y
      yield p
      stack.Push (x, (y - 1))
      stack.Push ((x + 1), y)
      stack.Push (x, (y + 1))
      stack.Push ((x - 1), y)
}

let scanGrid coords grid =
  let toX = coords |> mapMax (fun x -> x.X)
  let toY = coords |> mapMax (fun x -> x.Y)

  let handleCoord coord =
    let possiblePoints =
      buildPath (coord.X) (coord.Y) toX toY
      |> Seq.filter (fun (x, y) ->
          match Array2D.get grid x y with
          | [id] when id = coord.Id -> true
          | _ -> false)
      |> Array.ofSeq
    let isInfinite = possiblePoints |> Array.exists (fun (x, y) -> x = 0 || y = 0 || x = toX || y = toY)
    if isInfinite then None
    else
      let areaSize = possiblePoints |> Array.length
      Some areaSize

  Seq.choose handleCoord coords

let inputFileName = Path.Combine (__SOURCE_DIRECTORY__, "day06-input.txt")
let input = inputFileName |> File.ReadLines |> Seq.mapi parse |> Array.ofSeq
let grid = input |> buildGrid
let areas = scanGrid input grid
areas |> Seq.max

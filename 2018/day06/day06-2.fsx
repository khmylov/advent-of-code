#time "on";;
open System
open System.IO

type CoordId = int
type Coord = {Id: CoordId; X: int; Y: int}
let parse (id: CoordId) (str: string) =
  let s = str.Split ',' |> Array.map (Int32.Parse)
  {Id = id; X = s.[0]; Y = s.[1]}

let distance fromX fromY toX toY = abs (fromY - toY) + abs (fromX - toX)
let boundaries fn xs = (xs |> Seq.map fn |> Seq.min), (xs |> Seq.map fn |> Seq.max)

let inputFileName = Path.Combine (__SOURCE_DIRECTORY__, "day06-input.txt")
let input = inputFileName |> File.ReadLines |> Seq.mapi parse |> Array.ofSeq
let (xMin, xMax) = input |> boundaries (fun c -> c.X)
let (yMin, yMax) = input |> boundaries (fun c -> c.Y)

seq {
  for i in xMin..xMax do
    for j in yMin..yMax do
      let distSum = input |> Seq.map (fun c -> distance (c.X) (c.Y) i j) |> Seq.sum
      yield i, j, distSum
}
|> Seq.filter (fun (_,_, dist) -> dist < 10000)
|> Seq.length

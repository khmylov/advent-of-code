#time "on";;
open System
open System.IO
open System.Collections.Generic

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

let coordArea = new Dictionary<CoordId, (int*bool)>()
for i in xMin..xMax do
  for j in yMin..yMax do
    let distGroups = input |> Seq.groupBy (fun c -> distance (c.X) (c.Y) i j)
    let minGroup = distGroups |> Seq.minBy fst |> snd
    if (minGroup |> Seq.length) = 1 then
      let minDistCoord = Seq.head minGroup
      let key = minDistCoord.Id
      let isInfinite = (i = xMin || i = xMax || j = yMin || j = yMax)
      coordArea.[key] <-
        match coordArea.TryGetValue key with
        | true, (currArea, currIsInfinite) -> (currArea + 1, currIsInfinite || isInfinite)
        | _ -> 1, isInfinite

coordArea |> Seq.filter (fun pair -> not (snd pair.Value)) |> Seq.maxBy (fun x -> fst x.Value)

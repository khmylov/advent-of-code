open System.IO

module Seq =
  let foldi folder seed =
    Seq.mapi (fun i x -> i, x) >> Seq.fold (fun acc (i, x) -> folder acc i x) seed

let inline square x = x * x

// adjusted angle of vector from x1,y1 to x2,y2
let rec getAngle x1 y1 x2 y2 =
  let dx, dy = x2 - x1, y1 - y2 // yes, y1-y2 is correct
  let rads = atan2 (double dy) (double dx)
  let d1 = rads * 180.0 / System.Math.PI
  // mirror so that angle increases clock-wise, and also rotate by 90 degrees, so that UP is 0.
  if dx < 0 && dy >= 0 then 450.0 - d1 else 90.0 - d1

let rec getDistance x1 y1 x2 y2 = (square (x2 - x1) + square (y2 - y1)) |> double |> sqrt

let parseArea input =
  let rec parseLine y = Seq.foldi (fun set x c -> if c = '#' then Set.add (x, y) set else set)
  input |> Seq.foldi (fun set y line -> parseLine y set line) Set.empty

let inputFileName = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
let area = inputFileName |> File.ReadAllLines |> parseArea

let (x0, y0) = 25, 31 // from part1

// Order all points by angle from UP vector going clock-wise.
// If there are multiple vectors with the same angle, pick the shortest one for this iteration.
let part2 =
  let rec step acc = function
    | s when Set.isEmpty s -> acc
    | remaining ->
      let nextAsteroids =
        remaining
        |> Set.toSeq
        |> Seq.groupBy (fun (x, y) -> getAngle x0 y0 x y)
        |> Seq.sortBy fst
        |> Seq.map (
          snd
          >> Seq.sortBy (fun (x, y) -> getDistance x0 y0 x y)
          >> Seq.head
        )
        |> List.ofSeq

      step (acc@nextAsteroids) (Set.difference remaining (Set.ofList nextAsteroids))

  step [] (Set.remove (x0, y0) area)
  |> fun ls -> ls.[199]
  |> fun (x, y) -> x * 100 + y

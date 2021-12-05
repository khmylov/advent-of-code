open System.IO

type Content = | Ground | Tree | Lumber
type Grid = Content[,]

let toSeq grid = seq { for y in 0..Array2D.length2 grid - 1 do for x in 0..Array2D.length1 grid - 1 do yield x, y, grid.[x, y] }

let getNeighbours grid x y = seq {
  for y' in max (y - 1) 0 .. min (y + 1) (Array2D.length2 grid - 1) do
    for x' in max (x - 1) 0 .. min (x + 1) (Array2D.length1 grid - 1) do
      if (x', y') <> (x, y) then yield grid.[x', y']
}

let mutate (grid: Grid): Grid =
  let mutateOne (neighbours: Content list) = function
  | Ground when neighbours |> Seq.filter ((=) Tree) |> Seq.length >= 3 -> Tree
  | Tree when neighbours |> Seq.filter ((=) Lumber) |> Seq.length >= 3 -> Lumber
  | Lumber -> if Seq.contains Lumber neighbours && Seq.contains Tree neighbours then Lumber else Ground
  | x -> x
  
  grid |> Array2D.mapi (fun x y -> mutateOne (getNeighbours grid x y |> List.ofSeq))

let measure =
  toSeq
  >> Seq.fold (fun (trees, lumbs) (_, _, c) -> (if c = Tree then trees + 1 else trees), (if c = Lumber then lumbs + 1 else lumbs)) (0, 0)
  >> fun (x, y) -> x * y

let areEqual left right =
  Seq.zip (toSeq left) (toSeq right)
  |> Seq.exists (fun ((_, _, a), (_, _, b)) -> a <> b)
  |> not

let findLoop =
  let rec loop prevGrids grid =
    match List.tryFindIndex (areEqual grid) prevGrids with
    | None -> loop (grid::prevGrids) (mutate grid)
    | Some prevIndex -> (List.length prevGrids - prevIndex - 1), prevIndex + 1, grid
  loop []

let initialGrid =
  let inputLines = Path.Combine(__SOURCE_DIRECTORY__, "day18-input.txt") |> File.ReadAllLines
  Array2D.init (inputLines.[0].Length) (inputLines.Length) (fun x y -> match inputLines.[y].[x] with | '.' -> Ground | '|' -> Tree | '#' -> Lumber)

let (untilRepeat, loopLength, nextGrid) = findLoop initialGrid
let remainingIterations = (1000000000 - untilRepeat) % loopLength
printfn "After %d iterations, the mutation loops every %d iterations, so need to perform %d iterations" untilRepeat loopLength remainingIterations
let part2 = [1..remainingIterations] |> List.fold (fun g _ -> mutate g) nextGrid |> measure
// 195290

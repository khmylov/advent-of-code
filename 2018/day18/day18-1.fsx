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

let initialGrid =
  let inputLines = Path.Combine(__SOURCE_DIRECTORY__, "day18-input.txt") |> File.ReadAllLines
  Array2D.init (inputLines.[0].Length) (inputLines.Length) (fun x y -> match inputLines.[y].[x] with | '.' -> Ground | '|' -> Tree | '#' -> Lumber)

[1..10] |> List.fold (fun grid _ -> mutate grid) initialGrid |> measure

open System
open System.IO

let inputLines =
  Path.Combine(__SOURCE_DIRECTORY__, "input.txt") |> File.ReadAllLines

let HEIGHT = inputLines.Length
let WIDTH = inputLines[0].Length

let input =
  let updateMap map rowIdx colIdx c =
    if c >= '0' && c <= '9' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' then
      let newValue =
        match Map.tryFind c map with
        | Some ls -> (rowIdx, colIdx) :: ls
        | None -> [ rowIdx, colIdx ]

      Map.add c newValue map
    else
      map

  inputLines
  |> Seq.indexed
  |> Seq.fold
    (fun map (rowIdx, row) ->
      row
      |> Seq.indexed
      |> Seq.fold (fun map (colIdx, c) -> updateMap map rowIdx colIdx c) map)
    Map.empty
  |> Map.values

let isInGrid (row, col) =
  row >= 0 && col >= 0 && row < HEIGHT && col < WIDTH

let allPairs xs =
  seq {
    for x in xs do
      for y in xs do
        if x <> y then
          yield x, y
  }

// 247
let part1 =
  input
  |> Seq.collect allPairs
  |> Seq.map (fun ((row1, col1), (row2, col2)) -> row1 - (row2 - row1), col1 - (col2 - col1))
  |> Seq.filter isInGrid
  |> Seq.distinct
  |> Seq.length

// 861
let part2 =
  let antiNodeCoords ((row1, col1), (row2, col2)) =
    Seq.initInfinite (fun i -> row1 - i * (row2 - row1), col1 - i * (col2 - col1))
    |> Seq.takeWhile isInGrid

  input
  |> Seq.collect allPairs
  |> Seq.collect antiNodeCoords
  |> Seq.distinct
  |> Seq.length

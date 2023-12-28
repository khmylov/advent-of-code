open System
open System.IO
open System.Text.RegularExpressions

let input =
  Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines

let numberRegex = Regex("(\\d+)", RegexOptions.Compiled)

type Entry = { Value: int; ColumnBegin: int; ColumnEnd: int }

let rows = [|
  for line in input ->
    numberRegex.Matches line
    |> Seq.map (fun m -> { Value = Int32.Parse m.Value; ColumnBegin = m.Index; ColumnEnd = m.Index + m.Length })
    |> Array.ofSeq
|]

let inline isNearX x entry =
  entry.ColumnEnd = x || entry.ColumnBegin = x + 1 || (entry.ColumnBegin <= x && entry.ColumnEnd >= x)

let findAdjacent x y =
  [ y-1; y; y+1 ]
  |> Seq.choose (fun y' -> Array.tryItem y' rows)
  |> Seq.collect (Seq.filter (isNearX x))

let symbols = seq {
  for (y, line) in Seq.indexed input do
    for (x, char) in Seq.indexed line do
      if not (Char.IsDigit char || char = '.') then
        yield x, y, char
}

// 551094
let part1 =
  symbols
  |> Seq.collect (fun (x, y, _) -> findAdjacent x y)
  |> Seq.sumBy (_.Value)

// 80179647
let part2 =
  seq { for x, y, c in symbols do if c = '*' then yield findAdjacent x y |> List.ofSeq }
  |> Seq.sumBy (function | [v1; v2] -> v1.Value * v2.Value | _ -> 0)

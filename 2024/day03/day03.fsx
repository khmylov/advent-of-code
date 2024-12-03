open System.IO
open System.Text.RegularExpressions

let input = Path.Combine(__SOURCE_DIRECTORY__, "input.txt") |> File.ReadAllText

// 192767529
let part1 =
  input
  |> Regex("mul\((\d+),(\d+)\)").Matches
  |> Seq.sumBy (fun m -> int m.Groups[1].Value * int m.Groups[2].Value)

// 104083373
let part2 =
  input
  |> Regex("don't\(\)|do\(\)|mul\((\d+),(\d+)\)").Matches
  |> Seq.fold
    (fun (result, isEnabled) m ->
      match m.Value, isEnabled with
      | "don't()", _ -> result, false
      | "do()", _ -> result, true
      | _, true -> result + int m.Groups[1].Value * int m.Groups[2].Value, true
      | _ -> result, isEnabled)
    (0, true)
  |> fst

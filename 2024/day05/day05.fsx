open System
open System.IO

let updatesAndSorted =
  let lines = Path.Combine(__SOURCE_DIRECTORY__, "input.txt") |> File.ReadAllLines
  let splitIndex = lines |> Seq.findIndex (fun s -> s.Length = 0)

  let rules =
    lines[0 .. splitIndex - 1]
    |> Array.map (_.Split('|') >> Array.map Int32.Parse >> (fun ls -> ls[0], ls[1]))

  let comesBefore x y =
    rules |> Seq.exists (fun (x', y') -> x = x' && y = y')

  lines[splitIndex + 1 ..]
  |> Array.map (
    _.Split(',')
    >> Seq.map Int32.Parse
    >> List.ofSeq
    >> fun xs -> xs, xs |> List.sortWith (fun x y -> if comesBefore x y then -1 else 1)
  )

let middlePage xs = List.item (List.length xs / 2) xs

// 6267
let part1 =
  updatesAndSorted
  |> Seq.filter (fun (orig, sorted) -> orig = sorted)
  |> Seq.sumBy (fst >> middlePage)

// 5184
let part2 =
  updatesAndSorted
  |> Seq.filter (fun (orig, sorted) -> orig <> sorted)
  |> Seq.sumBy (snd >> middlePage)

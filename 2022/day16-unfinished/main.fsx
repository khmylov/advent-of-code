open Microsoft.FSharp.Core.Operators.Checked
open System
open System.IO
open System.Text.RegularExpressions

module String =
  let split (sep: string) (s: string) = s.Split([|sep|], StringSplitOptions.RemoveEmptyEntries)

let regex = Regex("Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)")

// Use valves' indices (in input order) instead of their names
let input =
  Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> Array.map (fun line ->
    regex.Matches(line).[0].Groups
    |> List.ofSeq
    |> function [_; x1; x2; x3] -> x1.Value, x2.Value |> Int32.Parse, x3.Value |> String.split ", ")
  |> fun lines ->
    let indexOf valve = lines |> Array.findIndex (fun (v', _, _) -> v' = valve)
    lines |> Array.map (fun (_, p, vs) -> p, vs |> Array.map indexOf)

let inline pressureOf i = input[i] |> fst
let inline tunnelsFrom i = input[i] |> snd

let memo = Collections.Generic.Dictionary<_, _>()

let rec solve valve openValves timeLeft =
  if timeLeft <= 0 then 0
  else
    let openValvesKey = openValves |> Seq.fold (fun acc v -> acc ||| (1UL <<< v)) 0UL
    let key = $"{valve}/{openValvesKey}/{timeLeft}"
    match memo.TryGetValue key with
    | true, res -> res
    | _ ->
      let currOpenScore = openValves |> Seq.sumBy pressureOf
      let mutable res = currOpenScore * timeLeft

      if pressureOf valve > 0 && not (Set.contains valve openValves) then
        let nextValves = Set.add valve openValves
        res <- max res (currOpenScore + solve valve nextValves (timeLeft - 1))

      for v in tunnelsFrom valve do
        res <- max res (currOpenScore + solve v openValves (timeLeft - 1))

      memo.Add(key, res)
      res

let part1 = solve 0 Set.empty 30
printfn "part1: %d" part1

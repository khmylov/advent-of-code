open System.IO

let (seed, rules) =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> fun lines ->
    lines.[0] |> List.ofSeq,
    lines |> Seq.skip 2 |> Seq.map (fun line -> let s = line.Split " -> " in (s.[0].[0], s.[0].[1]), s.[1].[0]) |> Map.ofSeq

// Track large polymer as individual (A,B) pairs with their current counts
// Also append special trailing symbol `-` to make it easier working with the last polymer's letter
let run iterations =
  let inline increment key by map =
    let value = map |> Map.tryFind key |> Option.defaultValue 0I
    map |> Map.add key (value + by)

  let step map =
    map
    |> Map.toSeq
    |> Seq.fold (fun map' ((a, b), count) ->
      rules
      |> Map.tryFind (a, b)
      |> Option.map (fun x -> map' |> increment (a, b) (-count) |> increment (a, x) count |> increment (x, b) count)
      |> Option.defaultValue map'
    ) map
    |> Map.filter (fun _ value -> value > 0I)

  let pairMap = Seq.append seed ['-'] |> Seq.pairwise |> Seq.map (fun key -> key, 1I) |> Map.ofSeq

  seq {1..iterations}
  |> Seq.fold (fun map _ -> step map) pairMap
  |> Map.toSeq
  |> Seq.groupBy (fun ((a, _), _) -> a)
  |> Seq.map (fun (_, counts) -> Seq.sumBy snd counts)
  |> Array.ofSeq
  |> fun arr -> Array.max arr - Array.min arr

// 2740
let part1 = run 10
// 2959788056211
let part2 = run 40

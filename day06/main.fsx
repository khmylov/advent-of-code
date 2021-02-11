open System.IO

let input =
    Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Seq.fold (fun (groups, group) line -> if line = "" then (group::groups, []) else (groups, line::group)) ([], [])
    |> function | (groups, []) -> groups | (groups, last) -> last::groups

// 6504
let part1 = input |> List.sumBy (Seq.collect id >> Set.ofSeq >> Set.count)

// 3351
let part2 = input |> List.sumBy (Seq.map Set.ofSeq >> Set.intersectMany >> Set.count)

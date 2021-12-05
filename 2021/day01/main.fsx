open System.IO

let input =
    Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Seq.map System.Int32.Parse

// 1791
let part1 =
    input
    |> Seq.pairwise
    |> Seq.filter (fun (x, y) -> y > x)
    |> Seq.length

// 1822
let part2 =
    input
    |> Seq.windowed 3
    |> Seq.map (Array.sum)
    |> Seq.pairwise
    |> Seq.filter (fun (x, y) -> y > x)
    |> Seq.length

open System.IO

type Series = int64 list

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Seq.map int64
    |> Seq.fold (fun acc x -> x::acc) []

let preambleLength = 25

// 15353384
let part1 =
    let rec isSum x values =
        Seq.allPairs values values
        |> Seq.exists (fun (a, b) -> a + b = x)

    let rec loop = function
    | xs when List.length xs <= preambleLength -> None
    | x::xs when not <| isSum x (List.take preambleLength xs) -> Some x
    | _::xs -> loop xs

    loop input |> Option.get

// 2466556
let part2 =
    let findContiguousSum target = function
    | [] -> None
    | head::tail ->
        let rec loop acc = function
        | [] -> None
        | x::xs ->
            let accumulatedSum = List.sum acc
            if accumulatedSum + x = target then Some (x::acc)
            elif accumulatedSum + x > target then None
            else loop (x::acc) xs

        loop [head] tail

    input
    |> Seq.unfold (function | [] -> None | _::tl as xs -> Some(xs, tl))
    |> Seq.choose (findContiguousSum part1)
    |> Seq.head
    |> fun ls -> List.max ls + List.min ls

open System.IO

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Seq.map int
    |> Seq.sort
    |> List.ofSeq

let deviceJoltage = input |> Seq.max |> (+) 3
let adapters = input @ [ deviceJoltage ]

// 1885
let p1 =
    let rec loop adapterChain lastOutput = function
    | [] -> adapterChain
    | adapterInput::remainingAdapters ->
        let diff = adapterInput - lastOutput
        if diff > 3 then adapterChain
        else loop (diff::adapterChain) adapterInput remainingAdapters

    let diffs = loop [] 0 adapters
    let count n = Seq.filter ((=) n) >> Seq.length

    (count 1 diffs) * (count 3 diffs)

// 2024782584832
let p2 =
    let memoized fn =
        let mutable memo = Map.empty
        let rec runner n =
            Map.tryFind n memo
            |> Option.defaultWith (fun () ->
                let result = fn runner n
                memo <- Map.add n result memo
                result
            )
        runner

    // Start from the final output.
    // Connections(N) = SUM(i in [1; 3], Connections(N-i))
    deviceJoltage
    |> memoized (fun self n ->
        adapters
        // Find which adapters can be plugged into the current one.
        |> Seq.filter (fun x -> n > x && n - x <= 3)
        |> Seq.map self
        |> Seq.sum
        // Special case for first adapter which is plugged directly into 0
        |> fun x -> if n <= 3 then x + 1L else x)

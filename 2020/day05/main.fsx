open System.IO

type Row = | F | B
type Column = | L | R
type Encoded = Row list * Column list
type Decoded = Decoded of row : int * column : int

let decodeRow =
    let folder (lower, upper) = function
    | F -> lower, (lower + upper) / 2 - 1
    | B -> (lower + upper) / 2 + 1, upper
    List.fold folder (0, 127)
    >> fst

let decodeColumn =
    let folder (lower, upper) = function
    | L -> lower, (lower + upper) / 2 - 1
    | R -> (lower + upper) / 2 + 1, upper
    List.fold folder (0, 7)
    >> fst

let decode (rows, columns) = Decoded(decodeRow rows, decodeColumn columns)

let parseLine (s: string) =
    s.[0..7] |> Seq.map (function | 'F' -> F | _ -> B) |> List.ofSeq,
    s.[7..] |> Seq.map (function | 'R' -> R | _ -> L) |> List.ofSeq

let seatIds =
    Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Seq.map (parseLine >> decode >> fun (Decoded(row, col)) -> row * 8 + col)
    |> Set.ofSeq

// part1: 818
let maxId = seatIds.MaximumElement

// part2: 559
seq {seatIds.MinimumElement..maxId}
|> Seq.find (fun i -> not <| Set.contains i seatIds)

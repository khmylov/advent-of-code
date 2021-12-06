open System
open System.IO

let (input, initialBoards) =
  let lines = Path.Combine (__SOURCE_DIRECTORY__, "input.txt") |> File.ReadAllLines
  let input = lines.[0].Split(",") |> Seq.map (Int32.Parse) |> List.ofSeq
  let boards =
    lines
    |> Seq.skip 1
    |> Seq.filter (fun s -> s.Length > 0)
    |> Seq.chunkBySize 5
    |> Seq.map (fun chunk ->
      chunk 
      |> Seq.map (fun line -> line.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.map (Int32.Parse))
      |> array2D
    )
    |> Array.ofSeq
  input, boards

let rows board = seq {for i in 0..Array2D.length1 board - 1 -> seq {for j in 0..Array2D.length2 board - 1 -> board.[i, j]}}
let columns board = seq {for j in 0..Array2D.length2 board - 1 -> seq {for i in 0..Array2D.length1 board - 1 -> board.[i, j]}}
let allTrue = Seq.map snd >> Seq.contains false >> not
let isWinning board =
  board |> rows |> Seq.exists allTrue
  || board |> columns |> Seq.exists allTrue
let applyNumber board number =
  board
  |> Array2D.iteri (fun i j (value, _) -> if value = number then board.[i, j] <- (value, true))

let score (board, number) =
  board
    |> rows
    |> Seq.collect id
    |> Seq.filter (snd >> (=) false)
    |> Seq.map fst
    |> Seq.sum
    |> (*) number

let playSteps =
  seq {
    let mutable boards = initialBoards |> Seq.map (Array2D.map (fun x -> x, false)) |> Set.ofSeq
    for number in input do
      let mutable winningBoards = Set.empty
      for board in boards do
        applyNumber board number
        if isWinning board then
          winningBoards <- Set.add board winningBoards
          yield (board, number)
      boards <- Set.difference boards winningBoards
  }
  |> List.ofSeq

// 89001
let part1 = playSteps |> List.head |> score

// 7296
let part2 = playSteps |> List.last |> score


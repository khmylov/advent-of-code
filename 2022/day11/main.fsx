open System
open System.Collections.Generic
open System.IO

module String =
  let split (separator: string) (s: string) = s.Split([|separator|], StringSplitOptions.RemoveEmptyEntries)
  let subAfter (c: string) (s: string) = (s.IndexOf c + c.Length) |> s.Substring

type Operand = | Number of int | Old
type Operation = | Add of Operand | Mult of Operand
type Program = { Operation: Operation; Division: int; IfTrue: int; IfFalse: int }

let parseSingle input =
  let lines = String.split "\n" input
  let initialState = lines[1] |> String.subAfter ": " |> String.split ", " |> Seq.map Int32.Parse |> List.ofSeq
  let program = {
    Operation =
      lines[2]
      |> String.subAfter "new = old "
      |> String.split " "
      |> function | [|op1; op2|] ->
          let operand = match op2 with | "old" -> Old | num -> num |> Int32.Parse |> Number
          match op1 with | "*" -> Mult operand | "+" -> Add operand
    Division = lines[3] |> String.subAfter "by " |> Int32.Parse
    IfTrue = lines[4] |> String.subAfter "monkey " |> Int32.Parse
    IfFalse = lines[5] |> String.subAfter "monkey" |> Int32.Parse
  }
  initialState, program

let parseFrom fileName =
  Path.Combine (__SOURCE_DIRECTORY__, fileName)
  |> File.ReadAllText
  |> String.split "\n\n"
  |> Array.map parseSingle

let input = parseFrom "input.txt"

let solve rounds divideBy =
  let states = input |> Seq.map (fst >> Seq.map int64 >> Queue) |> Array.ofSeq
  let programs = input |> Seq.map snd |> Array.ofSeq
  let totals = states |> Array.map (fun _ -> 0L)

  for round in 1..rounds do
    printfn "Round %d" round
    for index in 0..(programs.Length - 1) do
      let queue = Array.get states index
      let program = Array.get programs index
      let rec loop () =
        match queue.TryDequeue() with
        | false, _ -> ()
        | true, old ->
          totals[index] <- totals[index] + 1L
          let newLevel = match program.Operation with | Add(Number n) -> old + int64 n | Add Old -> old + old | Mult(Number n) -> old * int64 n | Mult Old -> old * old
          let newLevel = newLevel % divideBy
          states[if newLevel % int64 program.Division = 0L then program.IfTrue else program.IfFalse].Enqueue newLevel
          loop()
      loop()

  totals |> Seq.sortDescending |> Seq.take 2 |> Seq.reduce (*)

// 55944
let part1 = solve 20 3L

// 15117269860
let part2 =
  input
  |> Seq.map (fun (_, x) -> x.Division)
  |> Seq.distinct
  |> Seq.reduce (*)
  |> int64
  |> solve 10000

open System
open System.IO

type Instruction = | Noop | Add of int

let parseFrom fileName =
  Path.Combine(__SOURCE_DIRECTORY__, fileName)
  |> File.ReadAllLines
  |> Seq.map (function | "noop" -> Noop | line when line.StartsWith "addx " -> line.Substring 4 |> Int32.Parse |> Add)

type State = {Cycle: int; RegX: int}

let next ((x::_) as xs) = function | Noop -> x::xs | Add value -> (x + value)::x::xs

let states =
  parseFrom "input.txt"
  |> Seq.fold next [1]
  |> List.rev
  |> List.mapi (fun i x -> {Cycle = i + 1; RegX = x})

// 12520
let part1 =
  states
  |> Seq.filter (fun x -> (x.Cycle - 20) % 40 = 0)
  |> Seq.sumBy (fun x -> x.Cycle * x.RegX)

// EHPZPJGL
let part2 =
  let WIDTH = 40
  let screen = [| for _ in 1..6 -> [| for _ in 1..WIDTH -> '.'|] |]
  for {Cycle = cycle; RegX = regX} in states do
    let row, beamPos = (cycle - 1)  / WIDTH, (cycle - 1) % WIDTH
    if beamPos >= regX - 1 && beamPos <= regX + 1 then
      screen[row][beamPos] <- '#'

  String.Join(Environment.NewLine, screen |> Array.map (fun row -> String.Join("", row)))
  |> printfn "%s"

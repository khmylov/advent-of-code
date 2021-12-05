open System.IO

type Program = Map<int, int>
type ParameterMode = Pos | Im
type Parameter = ParameterMode * int
type Instruction = {Opcode: int; Params: Parameter[]}

module Program =
  let fromSeq xs = xs |> Seq.mapi (fun i x -> i, x) |> Map.ofSeq
  let item program pos = Map.find pos program
  let store program position value = Map.add position value program

  let opcode descriptor = descriptor % 100

  let paramCount = function
  | 1 | 2 -> 3
  | 3 | 4 -> 1
  | 99 -> 0
  | x -> failwithf "Unknown opcode %d" x

  let paramModes descriptor =
    let pCount = descriptor |> opcode |> paramCount
    let rec loop acc remaining =
      if List.length acc >= pCount then List.rev acc
      else
        let mode = if remaining > 0 then (if remaining % 10 = 0 then Pos else Im) else Pos
        loop (mode::acc) (remaining / 10)
    loop [] (descriptor / 100)

  let parseInstruction program position =
    let descriptor = item program position
    descriptor
    |> paramModes
    |> Seq.mapi (fun i m -> m, item program (position + i + 1))
    |> fun modes -> opcode descriptor, Array.ofSeq modes
  
  let evalParam program param =
    match fst param with | Pos -> item program (snd param) | Im -> snd param

  let evalInstruction program output opcode parameters =
    let evaled = parameters |> Array.map (evalParam program)
    match opcode with
    | 1 -> evaled.[0] + evaled.[1] |> store program (snd parameters.[2]), output
    | 2 -> evaled.[0] * evaled.[1] |> store program (snd parameters.[2]), output
    | 3 -> store program (snd parameters.[0]) 1, output
    | 4 -> program, evaled.[0]::output

let rec run program position output =
  let instruction = Program.parseInstruction program position
  let parameters = snd instruction
  match fst instruction with
  | 99 -> program, output
  | opcode ->
    let nextProgram, nextOutput = Program.evalInstruction program output opcode parameters
    let nextPos = position + Array.length parameters + 1
    run nextProgram nextPos nextOutput

let inputProgram =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllText
  |> fun s -> s.Split ([|','|])
  |> Seq.map int32
  |> Program.fromSeq

//let pr1 = Program.fromSeq [|1101; 100; -1; 4; 0|]
//let pr1 = Program.fromSeq [3; 0; 4; 0; 99]
run inputProgram 0 []
// part1: 12896948


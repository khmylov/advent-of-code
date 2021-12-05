open System.IO

type Program = Map<int, int>
type ParameterMode = Pos | Im
type Parameter = ParameterMode * int
type Op = Add = 1 | Mul = 2 | In = 3 | Out = 4 | JT = 5 | JF = 6 | LT = 7 | Eq = 8 | Halt = 99
type Instruction = {Opcode: Op; Params: Parameter[]}
type State = {Program: Program; Output: int list; Pointer: int}

module Program =
  let fromSeq xs = xs |> Seq.mapi (fun i x -> i, x) |> Map.ofSeq
  let item program pos = Map.find pos program
  let store program position value = Map.add position value program

  let opcode descriptor = enum<Op>(descriptor % 100)

  let paramCount = function
  | Op.Add | Op.Mul -> 3
  | Op.In | Op.Out -> 1
  | Op.JT | Op.JF -> 2
  | Op.LT | Op.Eq -> 3
  | Op.Halt -> 0
  | x -> failwithf "Unknown opcode %A" x

  let paramModes descriptor =
    Seq.initInfinite id
    |> Seq.scan (fun remaining _ -> remaining / 10) (descriptor / 100)
    |> Seq.map (fun d -> if d > 0 then (if d % 10 = 0 then Pos else Im) else Pos)
    |> Seq.take (descriptor |> opcode |> paramCount)
    |> List.ofSeq

  let parseInstruction program position =
    let descriptor = item program position
    descriptor
    |> paramModes
    |> Seq.mapi (fun i m -> m, item program (position + i + 1))
    |> fun modes -> opcode descriptor, Array.ofSeq modes
  
  let evalParam program (mode, value) = match mode with | Pos -> item program value | Im -> value

  let evalInstruction (state: State) opcode parameters =
    let {Program = program} = state
    let evaled = parameters |> Array.map (evalParam program)
    match opcode with
    | Op.Add -> {state with Program = evaled.[0] + evaled.[1] |> store program (snd parameters.[2])}
    | Op.Mul -> {state with Program = evaled.[0] * evaled.[1] |> store program (snd parameters.[2])}
    | Op.In -> {state with Program = store program (snd parameters.[0]) 5}
    | Op.Out -> {state with Output = evaled.[0]::state.Output}
    | Op.JT -> if evaled.[0] <> 0 then {state with Pointer = evaled.[1]} else state
    | Op.JF -> if evaled.[0] = 0 then {state with Pointer = evaled.[1]} else state
    | Op.LT ->
      let value = if evaled.[0] < evaled.[1] then 1 else 0
      {state with Program = store program (snd parameters.[2]) value}
    | Op.Eq ->
      let value = if evaled.[0] = evaled.[1] then 1 else 0
      {state with Program = store program (snd parameters.[2]) value}

let rec run state =
  let {Program = program; Pointer = position} = state
  let instruction = Program.parseInstruction program position
  match fst instruction with
  | Op.Halt -> state
  | opcode ->
    let parameters = snd instruction
    let nextState = Program.evalInstruction state opcode parameters
    run (if nextState.Pointer = position then {nextState with Pointer = position + Array.length parameters + 1} else nextState)

let inputProgram =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllText
  |> fun s -> s.Split ([|','|])
  |> Seq.map int32
  |> Program.fromSeq

//let pr1 = Program.fromSeq [|1101; 100; -1; 4; 0|]
//let pr1 = Program.fromSeq [3; 0; 4; 0; 99]
run {Program = inputProgram; Pointer = 0; Output = []}
// part1: 12896948
// part2: 7704130

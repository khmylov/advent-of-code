open System.IO

type Program = Map<int, int>
type ParameterMode = Pos | Im
type Parameter = ParameterMode * int
type Op = Add = 1 | Mul = 2 | In = 3 | Out = 4 | JT = 5 | JF = 6 | LT = 7 | Eq = 8 | Halt = 99
type Instruction = {Opcode: Op; Params: Parameter[]}
type State = {Program: Program; Output: int list; Input: int list; Pointer: int}

module Program =
  let fromSeq = Seq.mapi (fun i x -> i, x) >> Map.ofSeq
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
    | Op.In ->
      let inputValue::nextInput = state.Input
      {state with Program = store program (snd parameters.[0]) inputValue; Input = nextInput}
    | Op.Out -> {state with Output = evaled.[0]::state.Output}
    | Op.JT -> if evaled.[0] <> 0 then {state with Pointer = evaled.[1]} else state
    | Op.JF -> if evaled.[0] = 0 then {state with Pointer = evaled.[1]} else state
    | Op.LT ->
      let value = if evaled.[0] < evaled.[1] then 1 else 0
      {state with Program = store program (snd parameters.[2]) value}
    | Op.Eq ->
      let value = if evaled.[0] = evaled.[1] then 1 else 0
      {state with Program = store program (snd parameters.[2]) value}


let run program initialPhases =
  let rec loop state =
    let {Program = program; Pointer = position} = state
    let instruction = Program.parseInstruction program position
    match fst instruction with
    | Op.Halt -> state
    | opcode ->
      let parameters = snd instruction
      let nextState = Program.evalInstruction state opcode parameters
      loop (if nextState.Pointer = position then {nextState with Pointer = position + Array.length parameters + 1} else nextState)

  initialPhases |> List.fold (fun input phase ->
    let nextState = loop {Program = program; Pointer = 0; Input = [phase; input]; Output = []}
    nextState.Output.Head
  ) 0

let inputProgram =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllText
  |> fun s -> s.Split ([|','|])
  |> Seq.map int32
  |> Program.fromSeq

let rec permutations = function
| [] -> seq [List.empty]
| x::xs -> Seq.collect (insertions x) (permutations xs)
and insertions x = function
| [] -> [[x]]
| (y::ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))

permutations [0..4]
|> Seq.map (run inputProgram)
|> Seq.max

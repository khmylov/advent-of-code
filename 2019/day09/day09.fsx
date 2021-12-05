open System.IO

type Program = Map<int64, int64>
type ParameterMode = Position | Immediate | Relative
type Parameter = ParameterMode * int64
type Op = Add = 1 | Mul = 2 | In = 3 | Out = 4 | JumpIfTrue = 5 | JumpIfFalse = 6 | LessThan = 7 | Eq = 8 | RelOffset = 9 | Halt = 99
type Instruction = {Opcode: Op; Params: Parameter[]}
type State =
  {
    Program: Program
    Input: int64 list
    OutputReversed: int64 list
    Pointer: int64
    RelativeBase: int64
  }
  // When instruction parameter represents an address to store the eval result, it must be treated differently than usual "input" parameters
  // To quote day 5, "Parameters that an instruction writes to will never be in immediate mode"
  member this.GetStoreAddress (mode, value) =
    match mode with
    | Relative -> this.RelativeBase + value
    | _ -> value

  member this.StoreTo output valueToStore =
    let position = this.GetStoreAddress output
    // printfn "Store %d in %d" valueToStore position
    let nextProgram = Map.add position valueToStore (this.Program)
    {this with Program = nextProgram}

  member this.JumpTo position =
    // printfn "Jump to %d" position
    {this with Pointer = position}

module Instructions =
  let toOpcode descriptor = enum<Op>((int descriptor) % 100)

  let paramCount = function
  | Op.Add | Op.Mul -> 3
  | Op.In | Op.Out -> 1
  | Op.JumpIfTrue | Op.JumpIfFalse -> 2
  | Op.LessThan | Op.Eq -> 3
  | Op.RelOffset -> 1
  | Op.Halt -> 0
  | x -> failwithf "Unknown opcode %A" x

  let paramModes descriptor =
    Seq.initInfinite id
    |> Seq.scan (fun remaining _ -> remaining / 10L) (descriptor / 100L)
    |> Seq.map (fun d -> match int d % 10 with | 0 -> Position | 1 -> Immediate | 2 -> Relative | x -> failwithf "Unexpected parameter mode %d" x)
    |> Seq.take (descriptor |> toOpcode |> paramCount)
    |> List.ofSeq

module Program =
  let fromSeq = Seq.mapi (fun i x -> int64 i, x) >> Map.ofSeq
  let item program pos = Map.tryFind pos program |> Option.defaultValue 0L

  let parseInstruction program position =
    let descriptor = item program position
    descriptor
    |> Instructions.paramModes
    |> Seq.mapi (fun i m -> m, item program (position + int64 i + 1L))
    |> fun modes -> Instructions.toOpcode descriptor, Array.ofSeq modes
  
  let evalParam state (mode, value) =
    match mode with
    | Position -> item (state.Program) value
    | Immediate -> value
    | Relative -> item (state.Program) (value + state.RelativeBase)

  let evalInstruction (state: State) opcode parameters =
    // printfn "Start eval instruction opcode=%A, params=%A" opcode parameters
    let evaled = parameters |> Array.map (evalParam state)
    // printfn "Evaled params opcode=%A, evaled=%A, params=%A" opcode evaled parameters
    match opcode with
    | Op.Add -> evaled.[0] + evaled.[1] |> state.StoreTo parameters.[2]
    | Op.Mul -> evaled.[0] * evaled.[1] |> state.StoreTo parameters.[2]
    | Op.In ->
      let inputValue::restInput = state.Input
      let stored = state.StoreTo parameters.[0] inputValue
      {stored with Input = restInput}
    | Op.Out -> {state with OutputReversed = evaled.[0]::state.OutputReversed}
    | Op.JumpIfTrue -> if evaled.[0] <> 0L then state.JumpTo evaled.[1] else state
    | Op.JumpIfFalse -> if evaled.[0] = 0L then state.JumpTo evaled.[1] else state
    | Op.LessThan -> (if evaled.[0] < evaled.[1] then 1L else 0L) |> state.StoreTo parameters.[2]
    | Op.Eq -> (if evaled.[0] = evaled.[1] then 1L else 0L) |> state.StoreTo parameters.[2]
    | Op.RelOffset ->
      let nextRelBase = state.RelativeBase + evaled.[0]
      // printfn "Change relative base %d -> %d" state.RelativeBase nextRelBase
      {state with RelativeBase = nextRelBase}
    | x -> failwithf "Unexpected opcode %A" x

let rec run state =
  let {Program = program; Pointer = position} = state
  let instruction = Program.parseInstruction program position
  match fst instruction with
  | Op.Halt -> state
  | opcode ->
    let parameters = snd instruction
    let nextState = Program.evalInstruction state opcode parameters
    run (if nextState.Pointer = position then {nextState with Pointer = position + (Array.length parameters |> int64) + 1L} else nextState)

let inputProgram =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllText
  |> fun s -> s.Split ([|','|])
  |> Seq.map int64
  |> Program.fromSeq

// run {Program = inputProgram; Pointer = 0L; RelativeBase = 0L; Input = [1L]; OutputReversed = []}
// part1: 2955820355

run {Program = inputProgram; Pointer = 0L; RelativeBase = 0L; Input = [2L]; OutputReversed = []}
// part2: 46643

open System
open System.Text.RegularExpressions

let intersect (xs: 'a seq) (ys: 'a list) =
  seq {for x in xs do for y in ys do if x = y then yield x}
  |> Seq.distinct

type Registers = int[]
type InstructionF = Registers -> int -> int -> int
type Instruction = {Opcode: int; A: int; B: int; C: int}

let instructions: (string * InstructionF) list = [
  "addr", fun rs a b -> rs.[a] + rs.[b]
  "addi", fun rs a b -> rs.[a] + b
  "mulr", fun rs a b -> rs.[a] * rs.[b]
  "muli", fun rs a b -> rs.[a] * b
  "banr", fun rs a b -> rs.[a] &&& rs.[b]
  "bani", fun rs a b -> rs.[a] &&& b
  "borr", fun rs a b -> rs.[a] ||| rs.[b]
  "bori", fun rs a b -> rs.[a] ||| b
  "setr", fun rs a _ -> rs.[a]
  "seti", fun rs a _ -> a
  "gtir", fun rs a b -> if a > rs.[b] then 1 else 0
  "gtri", fun rs a b -> if rs.[a] > b then 1 else 0
  "gtrr", fun rs a b -> if rs.[a] > rs.[b] then 1 else 0
  "eqir", fun rs a b -> if a = rs.[b] then 1 else 0
  "eqri", fun rs a b -> if rs.[a] = b then 1 else 0
  "eqrr", fun rs a b -> if rs.[a] = rs.[b] then 1 else 0
]

type Input = {Before: Registers; Instruction: Instruction; After: Registers}

let (|RegEx|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None

let split (separator: string) (str: string) = str.Split([|separator|], System.StringSplitOptions.RemoveEmptyEntries)

let parseInstruction =
  split " " >> Array.map (Int32.Parse) >> fun ns -> {Opcode = ns.[0]; A = ns.[1]; B = ns.[2]; C = ns.[3]} 

let parse (lines: string[]): Input =
  match lines.[0], lines.[2] with
  | RegEx @"Before:\s+\[(.*)\]" [beforeStr], RegEx @"After:\s+\[(.*)\]" [afterStr] ->
    let before = beforeStr |> split ", " |> Array.map (Int32.Parse)
    let after = afterStr |> split ", " |> Array.map (Int32.Parse)
    {Before = before; After = after; Instruction = parseInstruction lines.[1]}

let test (input: Input) =
  instructions
  |> List.filter (fun (_, f) ->
    let rs = Array.copy input.Before
    rs.[input.Instruction.C] <- f rs (input.Instruction.A) (input.Instruction.B)
    System.Linq.Enumerable.SequenceEqual(rs, input.After))
  |> List.map fst

let allLines = 
  System.IO.Path.Combine (__SOURCE_DIRECTORY__, "day16-input.txt")
  |> System.IO.File.ReadAllLines
  |> Array.filter (fun x -> x.Length > 0)

let sampleInputs =
  allLines
  |> Seq.chunkBySize 3
  |> Seq.filter (fun xs -> xs.[0].StartsWith "Before:")
  |> Seq.map parse
  |> List.ofSeq

let buildOptions =
  List.fold (fun map input ->
    let existing = map |> Map.tryFind (input.Instruction.Opcode) |> Option.defaultValue []
    let next = existing@(test input) |> List.distinct
    map |> Map.add (input.Instruction.Opcode) next
  ) Map.empty<int, string list> 

let resolve =
  let rec loop acc options =
    match options |> Map.tryFindKey (fun _ ls -> List.length ls = 1) with
    | Some onlyOne ->
      let instruction = options |> Map.find onlyOne |> List.head
      let nextAcc = acc |> Map.add onlyOne instruction
      let nextOptions = options |> Map.map (fun _ ls -> ls |> List.except [instruction])
      loop nextAcc nextOptions
    | None -> acc

  loop (Map.empty<int, string>)

let run instructionMap =
  Seq.fold (fun (registers: int[]) line ->
    let instructionName = instructionMap |> Map.find line.Opcode
    instructions
    |> List.find (fun (name, _) -> name = instructionName)
    |> fun (_, f) -> registers.[line.C] <- f registers (line.A) (line.B)
    registers
  ) (Array.create 4 0)

let options = buildOptions sampleInputs
let resolved = resolve options

let programLines =
  allLines
  |> Array.skip (sampleInputs.Length * 3)
  |> Array.map parseInstruction
  
run resolved programLines

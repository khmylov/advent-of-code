open System
open System.Text.RegularExpressions

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

let parse (lines: string[]): Input =
  match lines.[0], lines.[2] with
  | RegEx @"Before:\s+\[(.*)\]" [beforeStr], RegEx @"After:\s+\[(.*)\]" [afterStr] ->
    let before = beforeStr |> split ", " |> Array.map (Int32.Parse)
    let after = afterStr |> split ", " |> Array.map (Int32.Parse)
    let ns = lines.[1] |> split " " |> Array.map (Int32.Parse)
    {Before = before; After = after; Instruction = {Opcode = ns.[0]; A = ns.[1]; B = ns.[2]; C = ns.[3]}}

let test (input: Input) =
  instructions
  |> List.filter (fun (_, f) ->
    let rs = Array.copy input.Before
    rs.[input.Instruction.C] <- f rs (input.Instruction.A) (input.Instruction.B)
    System.Linq.Enumerable.SequenceEqual(rs, input.After))
  |> List.map fst
  
System.IO.Path.Combine (__SOURCE_DIRECTORY__, "day16-input.txt")
|> System.IO.File.ReadAllLines
|> Seq.filter (fun x -> x.Length > 0)
|> Seq.chunkBySize 3
|> Seq.filter (fun xs -> xs.[0].StartsWith "Before:")
|> Seq.map (parse >> test)
|> Seq.filter (fun xs -> xs.Length >= 3)
|> Seq.length

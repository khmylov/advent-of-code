open System.IO
open Microsoft.FSharp.Core.Operators.Checked

type Registers = int[]
type InstructionF = Registers -> int -> int -> int
type Instruction = {Name: string; A: int; B: int; C: int}

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

let split (separator: string) (str: string) = str.Split([|separator|], System.StringSplitOptions.RemoveEmptyEntries)
let parseInt = System.Int32.Parse

let rec exec ipRegister registers =
  Array.tryItem (Array.get registers ipRegister)
  >> Option.map (fun instr ->
    let (_, func) = instructions |> List.find (fun (name, _) -> name = instr.Name)  
    registers.[instr.C] <- func registers instr.A instr.B
    registers.[ipRegister] <- registers.[ipRegister] + 1
  )

let runProgram ip program =
  let registers = [|0; 0; 0; 0; 0; 0|]
  let rec step() =
    match exec ip registers program with
    | Some _ -> step()
    | None -> registers.[0]

  step()

let ipRegister, instructionsInput =
  let lines = File.ReadAllLines <| Path.Combine(__SOURCE_DIRECTORY__, "day19-input.txt")

  let instr = 
    lines
    |> Seq.skip 1
    |> Seq.map (split " " >> fun xs -> {Name = xs.[0]; A = parseInt xs.[1]; B = parseInt xs.[2]; C = parseInt xs.[3]})
    |> Array.ofSeq

  let ipRegister = lines.[0].Substring("#ip".Length) |> parseInt
  ipRegister, instr

runProgram ipRegister instructionsInput // 1256

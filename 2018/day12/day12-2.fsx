module Seq =
  let toString xs = xs |> Array.ofSeq |> System.String

type Pot = {HasPlant: bool; Number: int}
let parseBool = ((=) '#')

type State = Pot list
module State =
  let parse = Seq.mapi (fun i c -> {HasPlant = parseBool c; Number = i}) >> List.ofSeq

  let isNormalized = function
  | ls when
    List.length ls >= 5 &&
    ls |> List.take 5 |> List.map (fun x -> x.HasPlant)
    |> (=) (List.replicate 5 false) -> true
  | _ -> false

  let prependWith indexDiff ls =
    let head = List.head ls
    {HasPlant = false; Number = head.Number + indexDiff}::ls

  // Ensure that there are always some leading and trailing "....."
  let normalize state =
    let mutable result = state
    while not <| isNormalized result do result <- prependWith -1 result
    result <- List.rev result
    while not <| isNormalized result do result <- prependWith 1 result
    List.rev result

  // Builds overlapping groups of 5 elements in state, to match them with rules
  let inFifth =
    let rec loop acc = function
    | ls when List.length ls < 5 -> acc
    | ls ->
      let append, rest = List.take 5 ls, List.skip 1 ls
      loop (append::acc) rest
    loop [] >> List.rev
  
  let plantNumbers = List.choose (fun x -> if x.HasPlant then Some(x.Number) else None)

  let toSymbols = Seq.map (fun x -> if x.HasPlant then '#' else '.') >> Seq.toString

type Rule = Pot list * bool
module Rule =
  let parse (str: string): Rule =
    let split = str.Split([|" => "|], System.StringSplitOptions.RemoveEmptyEntries)
    let pots = split.[0] |> Seq.mapi (fun i c -> {HasPlant = parseBool c; Number = i}) |> List.ofSeq
    let result = split.[1].[0] |> parseBool
    pots, result
  let matches pots rule =
    List.map (fun x -> x.HasPlant) (fst rule) = List.map (fun x -> x.HasPlant) pots

let runOne (rules: Rule list) (state: State): State =
  state
  |> State.normalize
  |> State.inFifth
  |> List.map (fun pots ->
    let newHasPlant =
      rules |> Seq.tryFind (fun r -> Rule.matches pots r)
      |> Option.map snd |> Option.defaultValue false
    {List.item 2 pots with HasPlant = newHasPlant}
  )

let run times rules initialState =
  // Patterns start repeating at some point,
  // with all "#" and "." just moving right one place at a time.
  // Finds the step number it happens at and also captures the state at that point.
  let rec findStablePoint number currentState =
    let newState = runOne rules currentState
    let currentSymbols, newSymbols = State.toSymbols currentState, State.toSymbols newState
    if newSymbols = currentSymbols then number, newState
    else findStablePoint (number + 1) newState
  let (stablePoint, lastState) = findStablePoint 1 initialState
  
  // After that, we have to count the remaining number of steps...
  let remaining = times - int64 stablePoint
  let plants = State.plantNumbers lastState
  // ... and add that number to all current pot numbers,
  // because from this point they just keep moving right
  plants |> List.map (fun x -> int64 x + remaining) |> List.sum

let inputFileName = System.IO.Path.Combine (__SOURCE_DIRECTORY__, "day12-input.txt")
let inputLines = System.IO.File.ReadAllLines inputFileName
let initialState = inputLines |> Seq.head |> State.parse
let rules = inputLines |> Seq.skip 2 |> Seq.map Rule.parse |> List.ofSeq

run 50000000000L rules initialState

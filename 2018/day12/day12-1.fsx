type Pot = bool * int
let parseBool = ((=) '#')
type State = Pot list
type Rule = bool list * bool
let parseRule (str: string) =
  let split = str.Split([|" => "|], System.StringSplitOptions.RemoveEmptyEntries)
  let pots = split.[0] |> Seq.map parseBool |> List.ofSeq
  let result = split.[1].[0] |> parseBool
  pots, result

let isNormalized = function
| ls when List.length ls >= 5 && ls |> List.take 5 |> List.map fst |> (=) [false; false; false; false; false] -> true
| _ -> false

let normalize list =
  let mutable result = list
  while not <| isNormalized result do
    let (_, index) = List.head result
    result <- (false, index-1)::result
  result <- List.rev result
  while not <| isNormalized result do
    let (_, index) = List.head result
    result <- (false, index+1)::result
  List.rev result

let inFifth =
  let rec loop acc = function
  | ls when List.length ls < 5 -> acc
  | ls ->
    let append = List.take 5 ls
    let rest = List.skip 1 ls
    loop (append::acc) rest
  loop [] >> List.rev

let runOne (rules: Rule list) state =
  state
  |> normalize
  |> inFifth
  |> List.mapi (fun i pots ->
    let plants = List.map fst pots
    let newPlant = 
      match rules |> Seq.tryFind (fun (pattern, _) -> pattern = plants) with
      | Some rule -> snd rule
      | None -> false
    newPlant, (List.item 2 pots |> snd)
  )

let run times rules state =
  Seq.replicate times ()
  |> Seq.fold (fun acc _ -> runOne rules acc) state

let inputFileName = System.IO.Path.Combine (__SOURCE_DIRECTORY__, "day12-input.txt")
let inputLines = System.IO.File.ReadAllLines inputFileName
let initialState = inputLines |> Seq.head |> Seq.mapi (fun i c -> parseBool c, i) |> List.ofSeq
let rules = inputLines |> Seq.skip 2 |> Seq.map parseRule |> List.ofSeq

run 20 rules initialState
|> Seq.choose (fun (hasPlant, index) -> if hasPlant then Some index else None)
|> Seq.sum

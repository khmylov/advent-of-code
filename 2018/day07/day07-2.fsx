open System.IO
open System.Text.RegularExpressions

let (|RegEx|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None

let parse = function
| RegEx @"^Step (\S) must be finished before step (\S) can begin\.$" [start; stop] -> (start.[0]), (stop.[0])
| s -> failwithf "Unable to parse %s" s

module Map =
  let addOrUpdate<'Key, 'T when 'Key: comparison> (key: 'Key) (addFunc: unit -> 'T) (updateFunc: 'T -> 'T) (map: Map<'Key, 'T>) =
    match Map.tryFind key map with
    | None -> Map.add key (addFunc()) map
    | Some value -> Map.add key (updateFunc value) map
  let choose fn = Map.toSeq >> Seq.choose (fun (key, value) -> fn key value)

let getDuration step = (int step) - (int 'A') + 61

let buildDependensOn =
  Seq.fold
    (fun map (start, stop) ->
      Map.addOrUpdate stop (fun () -> [start]) (fun ls -> start::ls) map)
    Map.empty<char, char list>

let workerCount = 5

type StepState = | NotStarted | InProgress of WorkerId:int * FinishAt:int | Completed
type Steps = Map<char, StepState>

let inputFileName = Path.Combine (__SOURCE_DIRECTORY__, "day07-input.txt")
let input = inputFileName |> File.ReadAllLines  |> Array.filter (fun x -> x.Length > 0) |> Array.map parse
let dependsOn = input |> buildDependensOn

let getNotStartedSteps =
  Map.choose (fun id c -> match c with | NotStarted -> Some id | _ -> None)
  >> List.ofSeq

let getInProgressSteps =
  Map.choose (fun s state -> match state with | InProgress (wId, finishAt) -> Some (s, wId, finishAt) | _ -> None)

let getCompletedSteps =
  Map.choose (fun id c -> match c with | Completed -> Some id | _ -> None)

let adjustForNow now =
  Map.map (fun _ s ->
    match s with | InProgress (_, finishAt) when finishAt = now -> Completed | _ -> s)

let findIdleWorker steps =
  let busyWorkers = steps |> getInProgressSteps |> Seq.map (fun (_, wId, _) -> wId)
  Seq.init workerCount id |> Seq.except busyWorkers |> Seq.tryHead

let findNextStep steps =
  let completedSteps = steps |> getCompletedSteps
  steps
  |> getNotStartedSteps
  |> List.tryFind (fun x ->
    match Map.tryFind x dependsOn with
    | None -> true
    | Some dependencies -> dependencies |> List.except completedSteps |> List.isEmpty)

let getWorkerState workerId =
  getInProgressSteps
  >> Seq.choose (fun (x, w, _) -> if w = workerId then x |> string |> Some else None)
  >> Seq.tryHead
  >> Option.defaultValue "."

let rec startNextSteps (now: int) (steps: Steps) =
  match getNotStartedSteps steps, findIdleWorker steps, findNextStep steps with
  | _::_, Some idleWorker, Some nextStep ->
    steps
    |> Map.add nextStep (InProgress (idleWorker, now + getDuration nextStep))
    |> startNextSteps now
  | _ -> steps

let build =
  let rec loop (now: int) (oldSteps: Steps) =
    let steps = adjustForNow now oldSteps
    match getNotStartedSteps steps with
    | [] -> steps |> getInProgressSteps |> Seq.map (fun (_, _, x) -> x) |> Seq.max
    | _ -> startNextSteps now steps |> loop (now + 1)

  Seq.map (fun c -> c, NotStarted) >> Map.ofSeq >> loop 0

input
|> Seq.map fst
|> Seq.append (input |> Seq.map snd)
|> Seq.distinct
|> Seq.sortBy id
|> List.ofSeq
|> build

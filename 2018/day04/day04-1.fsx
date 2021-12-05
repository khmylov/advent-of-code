open System
open System.IO
open System.Text.RegularExpressions
open System.Globalization

type GuardId = int
type Minute = int
type EventData = BeginShift of GuardId: GuardId | WakeUp | FallAsleep
type Event = DateTimeOffset * EventData

let (|RegEx|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None

let parse = function
| RegEx @"^\[(.+)\] (.+)$" [timeText; eventText] ->
  let time = DateTimeOffset.ParseExact(timeText, "yyyy-MM-dd HH:mm", CultureInfo.InvariantCulture)
  let event =
    match eventText with
    | "wakes up" -> WakeUp
    | "falls asleep" -> FallAsleep
    | RegEx @"Guard #(\d+) begins shift" [guardIdText] -> guardIdText |> Int32.Parse |> BeginShift
    | s -> failwithf "Unable to parse event %s" s
  (time, event)
| s -> failwithf "Unable to parse %s" s

module Map =
  let addOrUpdate<'Key, 'T when 'Key: comparison> (key: 'Key) (addFunc: unit -> 'T) (updateFunc: 'T -> 'T) (map: Map<'Key, 'T>) =
    match Map.tryFind key map with
    | None -> Map.add key (addFunc()) map
    | Some value -> Map.add key (updateFunc value) map

let getMinutes (start: DateTimeOffset) (finish: DateTimeOffset) =
  let rec aux acc date =
    if date >= finish then acc
    else aux (date.Minute::acc) (date.AddMinutes 1.0)
  aux [] start

let buildSleepMap (events: Event seq): Map<GuardId, Minute list> =
  let seedGuard: GuardId = events |> Seq.head |> snd |> function | BeginShift id -> id | _ -> failwith "Unexpected first event"
  let (_, _, res) =
    Seq.fold (fun (guardId, lastAsleep, map) (time, eventData) ->
      match eventData with
      | BeginShift nextId -> nextId, DateTimeOffset.MinValue, map
      | FallAsleep -> guardId, time, map
      | WakeUp ->
        let addedMinutes = getMinutes lastAsleep time
        let newMap = map |> Map.addOrUpdate guardId (fun () -> addedMinutes) (fun xs -> addedMinutes @ xs)
        guardId, DateTimeOffset.MinValue, newMap
    ) (seedGuard, DateTimeOffset.MinValue, Map.empty) events
  res

let inputFileName = Path.Combine (__SOURCE_DIRECTORY__, "day04-input.txt")
let input = inputFileName |> File.ReadAllLines |> Seq.filter (fun x -> x.Length > 0)

input
|> Seq.map parse
|> Seq.sortBy fst
|> buildSleepMap
|> Map.toSeq
|> Seq.maxBy (snd >> List.length)
|> fun (guardId, minutes) -> guardId * (minutes |> Seq.groupBy id |> Seq.maxBy (snd >> Seq.length) |> fst)

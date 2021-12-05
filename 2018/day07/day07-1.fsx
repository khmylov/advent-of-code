open System
open System.IO
open System.Text.RegularExpressions

let (|RegEx|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None

let parse = function
| RegEx @"^Step (\S) must be finished before step (\S) can begin\.$" [start; stop] -> start, stop
| s -> failwithf "Unable to parse %s" s

module Map =
  let addOrUpdate<'Key, 'T when 'Key: comparison> (key: 'Key) (addFunc: unit -> 'T) (updateFunc: 'T -> 'T) (map: Map<'Key, 'T>) =
    match Map.tryFind key map with
    | None -> Map.add key (addFunc()) map
    | Some value -> Map.add key (updateFunc value) map

module List =
  let without key = List.except [key]
  let toString separator ls = String.Join (separator, List.toSeq ls)

let buildDependensOn =
  Seq.fold
    (fun map (start, stop) ->
      Map.addOrUpdate stop (fun () -> [start]) (fun ls -> start::ls) map)
    Map.empty<string, string list>

let build steps dependsOn =
  let rec aux acc dependsOn = function
    | [] -> acc
    | steps ->
      let nextStep = steps |> Seq.find (fun s ->
        match Map.tryFind s dependsOn with | None | Some [] -> true | _ -> false)
      let newMap = Map.map (fun _ -> List.without nextStep) dependsOn
      aux (nextStep::acc) newMap (List.without nextStep steps)
  aux [] dependsOn steps |> List.rev |> List.toString ""

let inputFileName = Path.Combine (__SOURCE_DIRECTORY__, "day07-input.txt")
let input = File.ReadAllLines inputFileName |> Array.map parse
let steps = input |> Seq.map fst |> Seq.append (input |> Seq.map snd) |> Seq.groupBy id |> Seq.map fst |> Seq.sortBy id |> List.ofSeq
let dependsOn = input |> buildDependensOn
build steps dependsOn

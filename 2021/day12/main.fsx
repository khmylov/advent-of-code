open System.IO

module Map =
  let tryFindOrDefault key defaultValue = Map.tryFind key >> Option.defaultValue defaultValue
  let addOrUpdate key defaultValue fn map =
    let newValue = map |> Map.tryFind key |> Option.defaultValue defaultValue |> fn
    map |> Map.add key newValue
  let addToList key value = addOrUpdate key [] (fun ls -> value::ls)

let parse fileName =
  let toMap pairs = pairs |> Seq.groupBy fst |> Seq.map (fun (from, tos) -> from, tos |> Seq.map snd |> List.ofSeq)

  Path.Combine (__SOURCE_DIRECTORY__, fileName)
  |> File.ReadAllLines
  |> Array.map (fun line -> let split = line.Split "-" in split.[0], split.[1])
  |> Array.fold
    (fun map (from, to') ->
      map
      |> Map.addToList from to'
      |> fun m' -> if from <> "start" && to' <> "end" then m' |> Map.addToList to' from else m')
    Map.empty

let isSmall = String.forall (fun c -> let i = int c in i >= int 'a' && i <= int 'z')

let part1 input =
  let inline canVisit visitedSoFar cave = not <| (isSmall cave && visitedSoFar |> Set.contains cave)

  let rec loop visitedCaves currentPath =
    match List.head currentPath with
    | "end" -> [currentPath]
    | cave ->
      input
      |> Map.tryFindOrDefault cave []
      |> List.filter (canVisit visitedCaves)
      |> List.collect (fun c -> loop (if isSmall c then visitedCaves |> Set.add c else visitedCaves) (c::currentPath))

  loop (["start"] |> Set.ofSeq) ["start"]
  |> List.length

let example1 = parse "example1.txt" |> part1 // 10
let example2 = parse "example2.txt" |> part1 // 19
let part1' = parse "input.txt" |> part1 // 3802

let allCaves = 
  Map.toSeq
  >> Seq.collect (fun (k, v) -> seq {yield k; yield! v})
  >> Seq.distinct

let part2 input =
  let run allowedTwice =
    let inline canVisit visitMap cave =
      cave = "end"
      || cave |> isSmall |> not
      || visitMap |> Map.tryFindOrDefault cave 0 |> fun c -> c = 0 || cave = allowedTwice && c <= 2

    let rec loop visitedCaves currentPath =
      match List.head currentPath with
      | "end" -> [currentPath]
      | cave ->
        input
        |> Map.tryFindOrDefault cave []
        |> List.filter (canVisit visitedCaves)
        |> List.collect (fun c ->
          loop
            (if isSmall c then Map.addOrUpdate c 1 ((+) 1) visitedCaves else visitedCaves)
            (c::currentPath)
        )

    loop (["start", 1] |> Map.ofSeq) ["start"]

  input
  |> allCaves 
  |> Seq.filter (fun c -> c <> "start" && c <> "end" && isSmall c)
  |> Seq.distinct
  |> Seq.collect run
  |> Seq.distinct
  |> Seq.length

let p2ex1 = parse "example1.txt" |> part2 // 36
let p2ex2 = parse "example2.txt" |> part2 // 103
let part2' = parse "input.txt" |> part2 // 99448

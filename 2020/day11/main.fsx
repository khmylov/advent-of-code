open System.IO

let parse fileName =
  Path.Combine (__SOURCE_DIRECTORY__, fileName)
  |> File.ReadAllLines
  |> Array.map (Array.ofSeq)

let part1 fileName =
  let input = parse fileName
  let HEIGHT, WIDTH = input.Length, input.[0].Length

  let inline getNeighbours i j = seq {
    for i' in i-1..i+1 do
      for j' in j-1..j+1 do
        if (i', j') <> (i, j) && i' >= 0 && i' < HEIGHT && j' >= 0 && j' < WIDTH then
          yield input.[i'].[j']
  }

  let rec loop step =
    let newStates =
      input
      |> Seq.indexed
      |> Seq.collect (fun (i, xs) -> xs |> Seq.indexed |> Seq.map (fun (j, x) -> i, j, x))
      |> Seq.map (fun (i, j, seat) ->
        let neighbours = getNeighbours i j
        let newState = 
          match seat with
          | 'L' when neighbours |> Seq.forall ((<>) '#') -> '#'
          | '#' when (neighbours |> Seq.filter ((=) '#') |> Seq.length |> fun c -> c >= 4) -> 'L'
          | _ -> seat
        i, j, newState
      )
      |> List.ofSeq

    let mutable updateCount = 0
    for (i, j, s) in newStates do
      let current = input.[i].[j]
      if current <> s then
        updateCount <- updateCount + 1
        input.[i].[j] <- s

    if updateCount = 0 then input
    else loop (step + 1)

  loop 1
  |> Seq.collect id
  |> Seq.filter ((=) '#')
  |> Seq.length

let p1ex1 = part1 "example.txt" // 37
let p1 = part1 "input.txt" // 2277

let part2 fileName =
  let input = parse fileName
  let HEIGHT, WIDTH = input.Length, input.[0].Length

  let largeEnough fn = seq {for d in 1..1000 -> fn d}

  let inline getVisible i j = 
    [
      seq {for i' in i-1..-1..0 -> i', j}
      seq {for i' in i+1..HEIGHT -> i', j}
      seq {for j' in j-1..-1..0 -> i, j'}
      seq {for j' in j+1..WIDTH -> i, j'}
      largeEnough (fun d -> i+d, j+d)
      largeEnough (fun d -> i+d, j-d)
      largeEnough (fun d -> i-d, j+d)
      largeEnough (fun d -> i-d, j-d)
    ]
    |> Seq.choose (
      Seq.takeWhile (fun (i, j) -> i >= 0 && j >= 0 && i < HEIGHT && j < WIDTH)
      >> Seq.map (fun (i, j) -> input.[i].[j])
      >> Seq.tryFind (fun x -> x = '#' || x = 'L'))

  let rec loop step =
    let newStates =
      input
      |> Seq.indexed
      |> Seq.collect (fun (i, xs) -> xs |> Seq.indexed |> Seq.map (fun (j, x) -> i, j, x))
      |> Seq.map (fun (i, j, seat) ->
        let newState = 
          match seat with
          | 'L' when getVisible i j |> Seq.forall ((<>) '#') -> '#'
          | '#' when (getVisible i j |> Seq.filter ((=) '#') |> Seq.length |> fun c -> c >= 5) -> 'L'
          | _ -> seat
        i, j, newState
      )
      |> List.ofSeq

    let mutable updateCount = 0
    for (i, j, s) in newStates do
      let current = input.[i].[j]
      if current <> s then
        updateCount <- updateCount + 1
        input.[i].[j] <- s

    if updateCount = 0 then input
    else loop (step + 1)

  loop 1
  |> Seq.collect id
  |> Seq.filter ((=) '#')
  |> Seq.length

let p2ex1 = part2 "example.txt" // 26
let p2 = part2 "input.txt" // 2066

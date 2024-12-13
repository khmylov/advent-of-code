open System.IO

let input =
  Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> Array.map Array.ofSeq

let initialGuardPosition =
  Seq.indexed input
  |> Seq.choose (fun (j, row) -> row |> Seq.tryFindIndex ((=) '^') |> Option.map (fun i -> j, i))
  |> Seq.head

type Direction = Up | Down | Right | Left

let getAt row col =
  if row >= 0 && col >= 0 && row < input.Length && col < input[0].Length then
    Some(input[row][col])
  else
    None

let moveNext row col =
  function
  | Up ->
    match getAt (row - 1) col with
    | None -> None
    | Some '#' -> Some(row, col, Right)
    | _ -> Some(row - 1, col, Up)
  | Down ->
    match getAt (row + 1) col with
    | None -> None
    | Some '#' -> Some(row, col, Left)
    | Some _ -> Some(row + 1, col, Down)
  | Left ->
    match getAt row (col - 1) with
    | None -> None
    | Some '#' -> Some(row, col, Up)
    | _ -> Some(row, col - 1, Left)
  | Right ->
    match getAt row (col + 1) with
    | None -> None
    | Some '#' -> Some(row, col, Down)
    | _ -> Some(row, col + 1, Right)

let walkAround () =
  let rec loop row col dir visited =
    match moveNext row col dir with
    | None -> Some visited
    | Some(row', col', dir') ->
      if visited |> Set.contains (row', col', dir') then None
      else loop row' col' dir' (visited |> Set.add (row, col, dir))

  loop (fst initialGuardPosition) (snd initialGuardPosition) Up Set.empty

let initialPath =
  walkAround () |> Option.get |> Set.map (fun (row, col, _) -> row, col)

// 4602
let part1 = initialPath.Count + 1

// 1703
let part2 =
  let mutable result = 0

  for row, col in initialPath do
    input[row][col] <- '#'

    if walkAround () |> Option.isNone then
      result <- result + 1

    input[row][col] <- '.'

  result

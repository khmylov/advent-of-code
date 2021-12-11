open System.IO

let input =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> fun lines -> Array2D.init (lines.Length) (lines.[0].Length) (fun i j -> int lines.[i].[j] - int '0')

let neighboursOf i j array = seq {
  for i' in i-1..i+1 do
    for j' in j-1..j+1 do
      if (i', j') <> (i, j) && i' >= 0 && i' < Array2D.length1 array && j' >=0 && j' < Array2D.length2 array then
        yield i', j', array.[i', j']
}

module Array2D = 
  let toSeq xs = seq {
    for i in 0..Array2D.length1 xs - 1 do
      for j in 0..Array2D.length2 xs - 1 do
        yield i, j, xs.[i, j]
  }

let step input =
  input |> Array2D.iteri (fun i j value -> input.[i, j] <- value + 1)

  let readyToFlash = Array2D.toSeq >> Seq.filter (fun (_, _, v) -> v > 9) >> Seq.map (fun (i, j, _) -> i, j)

  let mutable queue = input |> readyToFlash |> Set.ofSeq
  let mutable flashed = Set.empty

  while queue.Count > 0 do
    let (i, j) = queue |> Seq.head
    let neighbours = neighboursOf i j input |> List.ofSeq

    for (i', j', v) in neighbours do
      input.[i', j'] <- v + 1
      if v + 1 > 9 && flashed |> Set.contains (i', j') |> not then
        queue <- queue |> Set.add (i', j')

    flashed <- flashed |> Set.add (i, j)
    queue <- queue |> Set.remove (i, j)

  for (i, j) in flashed do
    input.[i, j] <- 0
  
  flashed

// 1608
let part1 =
  let input = Array2D.copy input
  seq {1..100}
  |> Seq.sumBy (fun _ -> step input |> Set.count)

// 214
let part2 =
  let input = Array2D.copy input
  let mutable stepNumber = 0
  while input |> Array2D.toSeq |> Seq.map (fun (_, _, v) -> v) |> Seq.exists ((<>) 0) do
    stepNumber <- stepNumber + 1
    step input |> ignore
  stepNumber

open System.IO

let input =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> fun lines -> Array2D.init (lines.Length) (lines.[0].Length) (fun i j -> lines.[i].[j] |> fun c -> int c - int '0')

let HEIGHT, WIDTH = Array2D.length1 input, Array2D.length2 input

let areaOf i j =
  seq { 
    for (i', j') in [i-1, j; i+1, j; i, j-1; i, j+1] do
      if i' >= 0 && j' >= 0 && i' < HEIGHT && j' < WIDTH then
          yield i', j', input.[i', j'] 
  }

// Basins' bottom points reused for part1 and part2
let lowPoints =
  seq { for i in 0..HEIGHT-1 do for j in 0..WIDTH-1 -> i, j, input.[i, j]}
  |> Seq.filter (fun (i, j, height) -> areaOf i j |> Seq.forall (fun (_, _, x) -> height < x))
  |> List.ofSeq

// 516
let part1 = lowPoints |> List.sumBy (fun (_, _, height) -> height + 1)

// 1023660
let part2 =
  // BFS, keep a queue (actually Set here for simplicity) of items to be searched,
  // and track all already visited vertices
  // Would be more efficient to mark vertices as visited instead keeping them in a set but immutable solution runs well for input.
  let rec loop visited =
    Seq.collect (fun (i, j, height) ->
      areaOf i j
      |> Seq.filter (fun (_, _, height') -> height' <> 9 && height' >= height))
    >> Seq.filter (fun x -> not <| Set.contains x visited)
    >> Set.ofSeq
    >> fun newPoints ->
      if Set.isEmpty newPoints then visited
      else loop (Set.union visited newPoints) newPoints

  lowPoints
  |> Seq.map (fun x -> let init = set [x] in loop init init |> Set.count)
  |> Seq.sortDescending
  |> Seq.take 3
  |> Seq.reduce (*)

open System
open System.Text.RegularExpressions

let (|RegEx|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None
let parseInt = Int32.Parse

let boundaries fn xs = (xs |> Seq.map fn |> Seq.min), (xs |> Seq.map fn |> Seq.max)

type Vector = {X: int; Y: int}
type Point = {Position: Vector; Velocity: Vector}

let parse = function
| RegEx
    @"^position=\<\s*(.*),\s*(.*)\> velocity=\<\s*(.*),\s*(.*)\>$"
    [posX; posY; velX; velY] -> {
      Position = {X = parseInt posX; Y = parseInt posY}
      Velocity = {X = parseInt velX; Y = parseInt velY}
    }
| s -> failwithf "Unable to parse '%s'" s

let inline square x = x * x
let distance p1 p2 = sqrt ((square (float p2.X - float p1.X)) + (square (float p2.Y - float p1.Y)))

let transform count = Seq.map (fun point -> 
  let newPosition = {
    X = point.Position.X + count * point.Velocity.X
    Y = point.Position.Y + count * point.Velocity.Y
  }
  { point with Position = newPosition }
)

// Find "closeness" of a set of points
let measure points = 
  points
  |> Seq.map (fun p1 ->
    points
    |> Seq.filter (fun p2 -> not <| LanguagePrimitives.PhysicalEquality p1 p2)
    |> Seq.map (fun p2 -> distance p1.Position p2.Position)
    |> Seq.min)
  |> Seq.max

let run =
  let rec aux step points =
    match measure points with
    | 1.0 -> step, points
    | currentMeasure ->
      // While points are far apart, move them at a faster pace, and slow down when they get close
      let moveScale = if currentMeasure > 1000.0 then 100 else if currentMeasure > 100.0 then 10 else 1
      let newPoints = transform moveScale points |> List.ofSeq
      aux (step + moveScale) newPoints
  aux 0

let print points =
  let (minX, maxX) = points |> boundaries (fun x -> x.Position.X)
  let (minY, maxY) = points |> boundaries (fun x -> x.Position.Y)
  for y in minY..maxY do
    for x in minX..maxX do
      match points |> List.tryFind (fun p -> p.Position.X = x && p.Position.Y = y) with
      | Some _ -> Console.Write('#')
      | None -> Console.Write('.')
    Console.WriteLine()

System.IO.Path.Combine (__SOURCE_DIRECTORY__, "day10-input.txt")
|> System.IO.File.ReadAllLines
|> Seq.map parse
|> List.ofSeq
|> run
|> fun (steps, points) ->
  printfn "Steps: %d" steps
  print points

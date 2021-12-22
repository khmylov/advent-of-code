//let targetArea = (20, 30), (-10, -5) // example
let targetArea = (29, 73), (-248, -194) // input

type Vector = {X: int; Y: int}
type State = {Position: Vector; Velocity: Vector}

let simulate initialVelocity =
  let nextState ({Position = {X = x; Y = y}; Velocity = {X = velX; Y = velY}}) =
    {Position = {X = x + velX; Y = y + velY}; Velocity = {X = (if velX > 0 then velX - 1 elif velX < 0 then velX + 1 else 0); Y = velY - 1}}

  {Position = {X = 0; Y = 0}; Velocity = initialVelocity}
  |> Seq.unfold (fun state -> let next = nextState state in Some (next.Position, next))

let inline isOverShoot ({X = x; Y = y}) =
  let (_, maxX), (minY, _) = targetArea
  x > maxX || y < minY

let inline isInArea ({X = x; Y = y}) =
  let (minX, maxX), (minY, maxY) = targetArea
  x >= minX && x <= maxX && y >= minY && y <= maxY

let possibilities =
  [for x in 1..300 do for y in -300..300 do yield {X = x; Y = y}]
  |> List.choose (fun initialVelocity ->
    let path = 
      initialVelocity
      |> simulate 
      |> Seq.takeWhile (isOverShoot >> not) 
      |> List.ofSeq

    path
    |> List.tryLast 
    |> Option.filter isInArea
    |> Option.map (fun _ -> initialVelocity, path |> Seq.map (fun p -> p.Y) |> Seq.max)
  )

let part1 = possibilities |> Seq.map snd |> Seq.max // 30628
let part2 = possibilities |> List.length // 4433

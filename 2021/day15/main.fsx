open System
open System.IO

let initial =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> fun lines -> Array2D.init (lines.Length) (lines.[0].Length) (fun i j -> int lines.[i].[j] - int '0')

// Dijkstra's shortest path + min-heap
let solve initial =
  let HEIGHT, WIDTH = Array2D.length1 initial, Array2D.length2 initial

  let inline neighboursOf i j =
    [i+1,j; i,j+1; i-1,j; i, j-1]
    |> Seq.filter (fun (i', j') -> i' >= 0 && j' >= 0 && i' < HEIGHT && j' < WIDTH)

  let costs = initial |> Array2D.map (fun _ -> Int32.MaxValue)
  costs.[0, 0] <- 0
  let target = (HEIGHT - 1, WIDTH - 1)

  let queue = Collections.Generic.PriorityQueue<_, int>()
  queue.Enqueue((0, 0), 0)

  let rec step counter =
    if counter = 0 then ()
    else
      match queue.Dequeue() with
      | p when p = target -> ()
      | (i, j) ->
        let neighbours = neighboursOf i j
        for (y, x) in neighbours do
          let nCost = initial.[y, x] + costs.[i, j]
          if nCost < costs.[y, x] then
            costs.[y, x] <- nCost
            queue.Enqueue((y, x), nCost)
        step (counter - 1)

  step (HEIGHT * WIDTH - 1)
  costs.[fst target, snd target]

// 373
let part1 = solve initial

// 2868
let part2 =
  let items =
    let HEIGHT, WIDTH = Array2D.length1 initial, Array2D.length2 initial
    let data = Array2D.zeroCreate (HEIGHT * 5) (WIDTH * 5)
    for i in 0..HEIGHT - 1 do
      for j in 0..WIDTH - 1 do
        for stepY in 0..4 do
          for stepX in 0..4 do
            data.[stepY * HEIGHT + i, stepX * WIDTH + j] <- let newVal = initial.[i, j] + stepX + stepY in if newVal > 9 then newVal - 9 else newVal

    data

  solve items

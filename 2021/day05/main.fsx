open System
open System.IO

let input =
  Path.Combine (__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> Array.map (fun line ->
    let split = line.Split(" -> ") |> Array.collect (fun s -> s.Split(',') |> Array.map (Int32.Parse))
    (split.[0], split.[1]), (split.[2], split.[3])
  )

let inline increase (area: int[,]) x y = area.[y, x] <- area.[y, x] + 1

let score area =
  let mutable count = 0
  area |> Array2D.iter (fun x -> if x > 1 then count <- count + 1)
  count

// 6461
let part1 =
  let area = Array2D.zeroCreate 1000 1000
  for ((x1, y1), (x2, y2)) in input do
    if x1 = x2 then
      for j in min y1 y2..max y1 y2 do
        increase area x1 j
    elif y1 = y2 then
      for i in min x1 x2..max x1 x2 do
        increase area i y1
  
  score area

// 18065
let part2 =
  let area = Array2D.zeroCreate 1000 1000
  for ((x1, y1), (x2, y2)) in input do
    if x1 = x2 then
      for j in min y1 y2..max y1 y2 do
        increase area x1 j
    elif y1 = y2 then
      for i in min x1 x2..max x1 x2 do
        increase area i y1
    else
      if x1 < x2 then
        for di in 0..x2-x1 do
          increase area (x1 + di) (if y1 < y2 then y1 + di else y1 - di)
      else
        for di in 0..x1-x2 do
          increase area (x1 - di) (if y1 < y2 then y1 + di else y1 - di)

  score area

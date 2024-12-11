open System
open System.IO

let input =
  Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
  |> File.ReadAllLines
  |> Array.map Array.ofSeq

let WIDTH = input[0].Length
let HEIGHT = input.Length

let inline isValidCoord (x, y) =
  x >= 0 && y >= 0 && x < WIDTH && y < HEIGHT

let inline isChar c (x, y) = input[y][x] = c

let toWord =
  Seq.filter isValidCoord
  >> Seq.map (fun (x, y) -> input[y][x])
  >> Array.ofSeq
  >> String

let enumerateInput =
  seq {
    for y in 0 .. input.Length - 1 do
      for x in 0 .. input[y].Length - 1 do
        yield x, y
  }

// 2468
let part1 =
  enumerateInput
  |> Seq.filter (isChar 'X')
  |> Seq.collect (fun (x, y) ->
    seq {
      yield [ for d in 1..3 -> x + d, y ]
      yield [ for d in 1..3 -> x - d, y ]
      yield [ for d in 1..3 -> x, y + d ]
      yield [ for d in 1..3 -> x, y - d ]
      yield [ for d in 1..3 -> x + d, y + d ]
      yield [ for d in 1..3 -> x + d, y - d ]
      yield [ for d in 1..3 -> x - d, y + d ]
      yield [ for d in 1..3 -> x - d, y - d ]
    })
  |> Seq.map toWord
  |> Seq.filter ((=) "MAS")
  |> Seq.length

// 1864
let part2 =
  enumerateInput
  |> Seq.filter (isChar 'A')
  |> Seq.filter (fun (x, y) ->
    let w1 = toWord [ for d in -1 .. 1 -> x + d, y + d ]
    let w2 = toWord [ for d in -1 .. 1 -> x + d, y - d ]
    (w1 = "MAS" || w1 = "SAM") && (w2 = "MAS" || w2 = "SAM"))
  |> Seq.length
